{-# LANGUAGE OverloadedStrings #-}
-- | A minimal, pure-Haskell implementation of the Wayland wire protocol
-- (client side). It only supports what rivermonad needs: no file
-- descriptor passing, little-endian hosts.
--
-- Writes are buffered and must be sent with 'flush'. Reads happen in
-- 'readEvents', which is meant to be called from a single reader thread.
module Rivermonad.Wayland.Wire
  ( ObjectId (..)
  , nullObject
  , displayObject
  , Conn
  , connect
  , closeConn
  , flush
  , newObject
  , registerObject
  , forgetObject
  , sendRequest
  , queueFd
  , readMessages
  , RawMessage (..)
  , lookupInterface
    -- * Argument encoding
  , putInt, putUint, putFixed, putString, putObj, putArray
    -- * Argument decoding
  , getInt, getUint, getFixed, getStr, getObj, getArray
  ) where



import Control.Exception (throwIO, ErrorCall (..))
import Control.Monad (unless, when)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as BU
import Data.Binary.Get (Get, getWord32le, getInt32le, getByteString, skip)
import Data.Int (Int32)
import qualified Data.IntMap.Strict as IM
import Data.IORef
import Data.Word (Word16, Word32)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Ptr (Ptr)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import System.Environment (lookupEnv, unsetEnv)
import System.FilePath ((</>), isAbsolute)
import System.Posix.IO (closeFd, dup)
import System.Posix.Types (Fd (..))

newtype ObjectId = ObjectId { unObjectId :: Word32 }
  deriving (Eq, Ord)

instance Show ObjectId where
  show (ObjectId o) = '@' : show o

nullObject, displayObject :: ObjectId
nullObject = ObjectId 0
displayObject = ObjectId 1

data Conn = Conn
  { connSock    :: !NS.Socket
  , connNextId  :: !(IORef Word32)
  , connObjects :: !(IORef (IM.IntMap B.ByteString))
  , connOut     :: !(IORef BB.Builder)
  , connIn      :: !(IORef B.ByteString)
  , connFds     :: !(IORef [Fd])  -- ^ fds for the queued requests, oldest first (our dups)
  }

data RawMessage = RawMessage
  { rawSender :: !ObjectId
  , rawOpcode :: !Word16
  , rawBody   :: !B.ByteString
  }

-- | Connect to the compositor, honouring @WAYLAND_SOCKET@ and
-- @WAYLAND_DISPLAY@ like libwayland does.
connect :: IO Conn
connect = do
  sock <- lookupEnv "WAYLAND_SOCKET" >>= \case
    Just fdStr | [(fd, "")] <- reads fdStr -> do
      unsetEnv "WAYLAND_SOCKET"
      s <- NS.mkSocket fd
      NS.withFdSocket s NS.setCloseOnExecIfNeeded
      pure s
    _ -> do
      display <- maybe "wayland-0" id <$> lookupEnv "WAYLAND_DISPLAY"
      path <- if isAbsolute display
        then pure display
        else lookupEnv "XDG_RUNTIME_DIR" >>= \case
          Just dir -> pure (dir </> display)
          Nothing  -> throwIO (ErrorCall "XDG_RUNTIME_DIR is not set")
      s <- NS.socket NS.AF_UNIX NS.Stream NS.defaultProtocol
      NS.withFdSocket s NS.setCloseOnExecIfNeeded
      NS.connect s (NS.SockAddrUnix path)
      pure s
  Conn sock
    <$> newIORef 2
    <*> newIORef (IM.singleton 1 "wl_display")
    <*> newIORef mempty
    <*> newIORef B.empty
    <*> newIORef []

closeConn :: Conn -> IO ()
closeConn = NS.close . connSock

-- | Send every queued request to the compositor, with the file descriptors
-- they carry attached to the same message (libwayland takes them in order).
flush :: Conn -> IO ()
flush c = do
  out <- atomicModifyIORef' (connOut c) (\b -> (mempty, b))
  fds <- atomicModifyIORef' (connFds c) (\f -> ([], f))
  let bytes = BL.toStrict (BB.toLazyByteString out)
  if null fds
    then unless (B.null bytes) $ NSB.sendAll (connSock c) bytes
    else do
      r <- NS.withFdSocket (connSock c) $ \sock ->
        BU.unsafeUseAsCStringLen bytes $ \(p, len) ->
          withArrayLen [ fromIntegral f | Fd f <- fds ] $ \n arr ->
            c_send_fds sock p (fromIntegral len) arr (fromIntegral n)
      mapM_ closeFd fds
      when (r /= 0) $ throwIO (ErrorCall "rivermonad: sending file descriptors failed")

-- | Attach a file descriptor to the next request. The fd is duplicated, so
-- the caller keeps (and should close) its own.
queueFd :: Conn -> Fd -> IO ()
queueFd c fd = do
  d <- dup fd
  atomicModifyIORef' (connFds c) (\f -> (f ++ [d], ()))

foreign import ccall unsafe "rivermonad_send_fds"
  c_send_fds :: CInt -> CString -> CSize -> Ptr CInt -> CInt -> IO CInt

-- | Allocate a client-side object id for an interface. Ids are never reused,
-- which keeps the bookkeeping trivial (there are ~4 billion of them).
newObject :: Conn -> B.ByteString -> IO ObjectId
newObject c iface = do
  i <- atomicModifyIORef' (connNextId c) (\n -> (n + 1, n))
  when (i >= 0xff000000) $ throwIO (ErrorCall "rivermonad: out of client object ids")
  registerObject c (ObjectId i) iface
  pure (ObjectId i)

registerObject :: Conn -> ObjectId -> B.ByteString -> IO ()
registerObject c (ObjectId i) iface =
  atomicModifyIORef' (connObjects c) (\m -> (IM.insert (fromIntegral i) iface m, ()))

-- | Stop tracking an object. Events still in flight for it are dropped.
forgetObject :: Conn -> ObjectId -> IO ()
forgetObject c (ObjectId i) =
  atomicModifyIORef' (connObjects c) (\m -> (IM.delete (fromIntegral i) m, ()))

lookupInterface :: Conn -> ObjectId -> IO (Maybe B.ByteString)
lookupInterface c (ObjectId i) = IM.lookup (fromIntegral i) <$> readIORef (connObjects c)

-- | Queue a request. The header is: object id, then (size << 16 | opcode).
sendRequest :: Conn -> ObjectId -> Word16 -> BB.Builder -> IO ()
sendRequest c (ObjectId o) op body = do
  let payload = BL.toStrict (BB.toLazyByteString body)
      size = 8 + B.length payload
      msg = BB.word32LE o
         <> BB.word32LE ((fromIntegral size `shiftL` 16) .|. fromIntegral op)
         <> BB.byteString payload
  atomicModifyIORef' (connOut c) (\b -> (b <> msg, ()))

-- | Block until at least one complete message is available and return all
-- complete messages read so far. Throws on end of stream.
readMessages :: Conn -> IO [RawMessage]
readMessages c = do
  chunk <- NSB.recv (connSock c) 65536
  when (B.null chunk) $ throwIO (ErrorCall "rivermonad: compositor closed the connection")
  buf <- (<> chunk) <$> readIORef (connIn c)
  let (msgs, rest) = split buf
  writeIORef (connIn c) rest
  if null msgs then readMessages c else pure msgs
  where
    split bs
      | B.length bs < 8 = ([], bs)
      | otherwise =
          let o = word32At bs 0
              w = word32At bs 4
              size = fromIntegral (w `shiftR` 16)
              op = fromIntegral (w .&. 0xffff)
          in if size < 8
               then error "rivermonad: malformed Wayland message"
               else if B.length bs < size
                 then ([], bs)
                 else
                   let (m, rest) = B.splitAt size bs
                       (ms, rest') = split rest
                   in (RawMessage (ObjectId o) op (B.drop 8 m) : ms, rest')

word32At :: B.ByteString -> Int -> Word32
word32At bs i =
  fromIntegral (BU.unsafeIndex bs i)
    .|. (fromIntegral (BU.unsafeIndex bs (i + 1)) `shiftL` 8)
    .|. (fromIntegral (BU.unsafeIndex bs (i + 2)) `shiftL` 16)
    .|. (fromIntegral (BU.unsafeIndex bs (i + 3)) `shiftL` 24)

------------------------------------------------------------------------------
-- Encoding

putInt :: Int32 -> BB.Builder
putInt = BB.int32LE

putUint :: Word32 -> BB.Builder
putUint = BB.word32LE

putFixed :: Double -> BB.Builder
putFixed d = BB.int32LE (round (d * 256))

putObj :: ObjectId -> BB.Builder
putObj (ObjectId o) = BB.word32LE o

-- | Strings are length-prefixed (including the NUL terminator) and padded
-- to a multiple of 4 bytes.
putString :: B.ByteString -> BB.Builder
putString s =
  let len = B.length s + 1
  in BB.word32LE (fromIntegral len) <> BB.byteString s <> BB.word8 0 <> padding len

putArray :: B.ByteString -> BB.Builder
putArray a =
  let len = B.length a
  in BB.word32LE (fromIntegral len) <> BB.byteString a <> padding len

padding :: Int -> BB.Builder
padding len = mconcat (replicate ((4 - len `mod` 4) `mod` 4) (BB.word8 0))

------------------------------------------------------------------------------
-- Decoding

getInt :: Get Int32
getInt = getInt32le

getUint :: Get Word32
getUint = getWord32le

getFixed :: Get Double
getFixed = (/ 256) . fromIntegral <$> getInt32le

getObj :: Get ObjectId
getObj = ObjectId <$> getWord32le

getStr :: Get B.ByteString
getStr = do
  len <- fromIntegral <$> getWord32le
  if len == 0
    then pure B.empty
    else do
      s <- getByteString (len - 1)
      skip (1 + (4 - len `mod` 4) `mod` 4)
      pure s

getArray :: Get B.ByteString
getArray = do
  len <- fromIntegral <$> getWord32le
  a <- getByteString len
  skip ((4 - len `mod` 4) `mod` 4)
  pure a
