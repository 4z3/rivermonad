-- | IPC socket, modelled after niri's: newline-delimited JSON requests.
--
-- * @"EventStream"@: reply @{"Ok":"Handled"}@, then one
--   @{"State":{...}}@ line whenever the state changes (see
--   'Rivermonad.Engine.publishState' for the fields).
-- * @{"Action":A}@: reply @{"Ok":"Handled"}@ and run A, where A is one of
--   @{"FocusWorkspace":"3"}@, @{"ViewWorkspace":"3"}@, @"NextLayout"@,
--   @{"JumpToLayout":"Full"}@, @"FocusUp"@, @"FocusDown"@, @"Restart"@,
--   @"Logout"@ (close every window, then end the session),
--   @"NextKeyboardLayout"@, @"Quit"@ (save the state and stop managing;
--   the windows stay open for the next window manager process),
--   @{"FocusWindow":ID}@, @{"ShiftWindow":[ID,"3"]}@,
--   @{"AddWorkspace":"mail"}@, @{"RemoveWorkspace":"mail"}@ (only if
--   empty), @{"RenameWorkspace":["mail","post"]}@ and
--   @{"SetWorkspaces":["1","2","post"]}@ (see
--   "Rivermonad.Actions.DynamicWorkspaces").
-- * @"Snapshot"@: reply with one @{"Snapshot":{...}}@ line listing every
--   workspace with its windows and their geometry (see
--   'Rivermonad.Engine.publishSnapshot'), then close. Actions sent before
--   it are already applied in the reply.
--
-- The socket is @$XDG_RUNTIME_DIR/rivermonad-$WAYLAND_DISPLAY.sock@ and is
-- exported to children as @RIVERMONAD_SOCKET@.
module Rivermonad.IPC
  ( IPCServer
  , ipcSocketPath
  , startIPCServer
  , publishIPC
  , storeSnapshot
  , parseAction
  , runIPCClient
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, writeChan)
import Control.Concurrent.MVar
import qualified Control.Exception as E
import Control.Monad (filterM, forever, unless, void)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import Data.Maybe (fromMaybe)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import System.Directory (doesPathExist, removeFile)
import System.Environment (lookupEnv, setEnv)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import System.Timeout (timeout)

import Rivermonad.Core (IPCAction (..), Input (..))
import qualified Rivermonad.JSON as J

data IPCServer = IPCServer
  { srvSubs :: MVar [NS.Socket]
  , srvLast :: IORef B.ByteString
  , srvSnapshot :: IORef B.ByteString
  }

-- | The socket of the rivermonad managing /this/ Wayland display, if it
-- exists; @RIVERMONAD_SOCKET@ only as a fallback. The variable alone is not
-- trusted: in a session nested inside another rivermonad it is inherited
-- from the outer one.
ipcSocketPath :: IO FilePath
ipcSocketPath = do
  own <- defaultPath
  ownExists <- doesPathExist own
  env <- lookupEnv "RIVERMONAD_SOCKET"
  pure $ case env of
    Just p | not ownExists, not (null p) -> p
    _ -> own

defaultPath :: IO FilePath
defaultPath = do
  dir <- fromMaybe "/tmp" <$> lookupEnv "XDG_RUNTIME_DIR"
  disp <- fromMaybe "wayland-0" <$> lookupEnv "WAYLAND_DISPLAY"
  pure (dir </> ("rivermonad-" ++ map (\c -> if c == '/' then '_' else c) disp ++ ".sock"))

startIPCServer :: Chan Input -> IO (Maybe IPCServer)
startIPCServer ch = do
  r <- E.try $ do
    path <- defaultPath
    _ <- E.try (removeFile path) :: IO (Either E.IOException ())
    sock <- NS.socket NS.AF_UNIX NS.Stream NS.defaultProtocol
    NS.withFdSocket sock NS.setCloseOnExecIfNeeded
    NS.bind sock (NS.SockAddrUnix path)
    NS.listen sock 16
    setEnv "RIVERMONAD_SOCKET" path
    srv <- IPCServer <$> newMVar [] <*> newIORef B.empty <*> newIORef B.empty
    void $ forkIO $ forever $ do
      (client, _) <- NS.accept sock
      NS.withFdSocket client NS.setCloseOnExecIfNeeded
      void (forkIO (handleClient srv ch client))
    pure srv
  case r of
    Right srv -> pure (Just srv)
    Left e -> do
      hPutStrLn stderr ("rivermonad: IPC disabled: " ++ show (e :: E.SomeException))
      pure Nothing

handleClient :: IPCServer -> Chan Input -> NS.Socket -> IO ()
handleClient srv ch s = do
  line <- readLine s B.empty
  case line >>= J.parseUtf8 of
    Just (J.JStr "EventStream") ->
      -- Register under the lock so no state line is lost or duplicated.
      modifyMVar_ (srvSubs srv) $ \subs -> do
        lastLine <- readIORef (srvLast srv)
        ok <- sendLine s (J.renderUtf8 (J.obj [("Ok", J.str "Handled")]))
        ok' <- if ok && not (B.null lastLine) then sendRaw s lastLine else pure ok
        pure (if ok' then s : subs else subs)
    Just (J.JStr "Snapshot") -> do
      -- Wait until the requests sent before this one have been applied, so
      -- a client reads its own changes. Do not wait forever: without a
      -- screen there is no manage sequence to wait for.
      applied <- newEmptyMVar
      writeChan ch (InSync (void (tryPutMVar applied ())))
      _ <- timeout 1000000 (takeMVar applied)
      snap <- readIORef (srvSnapshot srv)
      void $ if B.null snap
        then sendLine s (J.renderUtf8 (J.obj [("Err", J.str "no snapshot yet")]))
        else sendLine s snap
      NS.close s
    Just (J.JObj [("Action", a)]) | Just act <- parseAction a -> do
      writeChan ch (InAction act)
      void (sendLine s (J.renderUtf8 (J.obj [("Ok", J.str "Handled")])))
      NS.close s
    _ -> do
      void (sendLine s (J.renderUtf8 (J.obj [("Err", J.str "unknown request")])))
      NS.close s

parseAction :: J.JValue -> Maybe IPCAction
parseAction v = case v of
  J.JStr "NextLayout" -> Just IPCNextLayout
  J.JStr "FocusUp" -> Just IPCFocusUp
  J.JStr "FocusDown" -> Just IPCFocusDown
  J.JStr "Restart" -> Just IPCRestart
  J.JStr "NextKeyboardLayout" -> Just IPCNextKeyboardLayout
  J.JStr "Logout" -> Just IPCLogout
  J.JObj [("FocusWorkspace", J.JStr t)] -> Just (IPCFocusWorkspace t)
  J.JObj [("ViewWorkspace", J.JStr t)] -> Just (IPCGreedyViewWorkspace t)
  J.JObj [("JumpToLayout", J.JStr l)] -> Just (IPCJumpToLayout l)
  J.JStr "Quit" -> Just IPCQuit
  J.JObj [("FocusWindow", J.JNum n)] -> IPCFocusWindow <$> windowId n
  J.JObj [("ShiftWindow", J.JArr [J.JNum n, J.JStr t])] -> (`IPCShiftWindow` t) <$> windowId n
  J.JObj [("AddWorkspace", J.JStr t)] -> Just (IPCAddWorkspace t)
  J.JObj [("RemoveWorkspace", J.JStr t)] -> Just (IPCRemoveWorkspace t)
  J.JObj [("RenameWorkspace", J.JArr [J.JStr old, J.JStr new])] -> Just (IPCRenameWorkspace old new)
  J.JObj [("SetWorkspaces", J.JArr ts)] -> IPCSetWorkspaces <$> mapM name ts
  _ -> Nothing

  where
    name (J.JStr t) = Just t
    name _ = Nothing
    windowId n
      | n >= 0, n <= 4294967295, n == fromIntegral (round n :: Integer) = Just (round n)
      | otherwise = Nothing

-- | Replace the reply to @"Snapshot"@ requests.
storeSnapshot :: IPCServer -> B.ByteString -> IO ()
storeSnapshot srv = writeIORef (srvSnapshot srv)

-- | Send a state line to every subscriber, if it changed.
publishIPC :: IPCServer -> B.ByteString -> IO ()
publishIPC srv body = do
  let line = body <> BC.pack "\n"
  prev <- readIORef (srvLast srv)
  unless (line == prev) $ modifyMVar_ (srvSubs srv) $ \subs -> do
    writeIORef (srvLast srv) line
    filterM (`sendRaw` line) subs

sendLine :: NS.Socket -> B.ByteString -> IO Bool
sendLine s b = sendRaw s (b <> BC.pack "\n")

sendRaw :: NS.Socket -> B.ByteString -> IO Bool
sendRaw s b = do
  r <- E.try (NSB.sendAll s b) :: IO (Either E.IOException ())
  case r of
    Right () -> pure True
    Left _ -> NS.close s >> pure False

-- | Read up to the first newline (at most 64 KiB).
readLine :: NS.Socket -> B.ByteString -> IO (Maybe B.ByteString)
readLine s acc
  | B.length acc > 65536 = pure Nothing
  | otherwise = case BC.elemIndex '\n' acc of
      Just i -> pure (Just (B.take i acc))
      Nothing -> do
        r <- E.try (NSB.recv s 4096) :: IO (Either E.IOException B.ByteString)
        case r of
          Right chunk | not (B.null chunk) -> readLine s (acc <> chunk)
          _ -> pure (if B.null acc then Nothing else Just acc)

-- | @rivermonad --ipc REQUEST@: send one request and print the replies
-- (an event stream keeps printing until interrupted).
runIPCClient :: String -> IO ()
runIPCClient req = do
  path <- ipcSocketPath
  r <- E.try $ E.bracket (NS.socket NS.AF_UNIX NS.Stream NS.defaultProtocol) NS.close $ \s -> do
    NS.connect s (NS.SockAddrUnix path)
    NSB.sendAll s (BC.pack (req ++ "\n"))
    let loop = do
          chunk <- NSB.recv s 4096
          unless (B.null chunk) $ do
            B.putStr chunk
            hFlush stdout
            loop
    loop
  case r of
    Right () -> pure ()
    Left e -> do
      hPutStrLn stderr ("rivermonad: cannot reach " ++ path ++ ": " ++ show (e :: E.IOException))
      exitFailure
