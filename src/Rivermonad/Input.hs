-- | Keyboard layout, key repeat, touchpad and mouse settings, through river's
-- river-input-management, river-xkb-config and river-libinput-config
-- protocols. Configured with 'input' in the 'XConfig'.
module Rivermonad.Input
  ( handleInputEvent
  , setupKeymap
  , nextKeyboardLayout
  , setKeyboardLayout
  , currentKeyboardLayout
  ) where

import Control.Monad (forM_, unless, when)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt (..))
import System.IO (hPutStrLn, stderr)
import System.Posix.IO (closeFd)
import System.Posix.Types (Fd (..))

import Rivermonad.Core
import Rivermonad.Wayland.Protocol
import Rivermonad.Wayland.Wire

foreign import ccall safe "rivermonad_keymap_fd"
  c_keymap_fd :: CString -> CString -> CString -> CString -> CString -> IO CInt

withConn :: (Conn -> IO a) -> X a
withConn f = asks conn >>= io . f

logErr :: String -> X ()
logErr = io . hPutStrLn stderr . ("rivermonad: " ++)

-- | Handle an input-related event; False if the event is not one of ours.
handleInputEvent :: Event -> X Bool
handleInputEvent ev = case ev of
  EvRiverInputManagerV1 _ (RiverInputManagerV1InputDevice d) -> do
    modify $ \s -> s { inputDevices = M.insert d (InputDev Nothing "" False) (inputDevices s) }
    done
  EvRiverInputManagerV1 {} -> done
  EvRiverInputDeviceV1 d e -> case e of
    RiverInputDeviceV1Type t -> updDev d (\x -> x { idType = Just t }) >> done
    RiverInputDeviceV1Name n -> updDev d (\x -> x { idName = utf8 n }) >> done
    RiverInputDeviceV1Done -> configureDevice d >> done
    RiverInputDeviceV1Removed -> do
      withConn (\c -> riverInputDeviceV1Destroy c d)
      modify $ \s -> s { inputDevices = M.delete d (inputDevices s) }
      done

  EvRiverXkbConfigV1 _ (RiverXkbConfigV1XkbKeyboard k) -> do
    modify $ \s -> s { xkbKeyboards = M.insert k (XkbKb (0, "") False False) (xkbKeyboards s) }
    done
  EvRiverXkbConfigV1 {} -> done
  EvRiverXkbKeymapV1 m RiverXkbKeymapV1Success -> do
    modify $ \s -> s { keymapState = KeymapReady m }
    ks <- gets (M.keys . xkbKeyboards)
    mapM_ applyKeymap ks
    done
  EvRiverXkbKeymapV1 m (RiverXkbKeymapV1Failure msg) -> do
    logErr ("river rejected the keymap: " ++ utf8 msg)
    withConn (\c -> riverXkbKeymapV1Destroy c m)
    modify $ \s -> s { keymapState = KeymapFailed }
    done
  EvRiverXkbKeyboardV1 k e -> case e of
    RiverXkbKeyboardV1Layout i n -> do
      updKb k (\x -> x { xkLayout = (fromIntegral i, utf8 n) })
      done
    RiverXkbKeyboardV1Done -> applyKeymap k >> applyNumlock k >> done
    RiverXkbKeyboardV1Removed -> do
      withConn (\c -> riverXkbKeyboardV1Destroy c k)
      modify $ \s -> s { xkbKeyboards = M.delete k (xkbKeyboards s) }
      done
    _ -> done

  EvRiverLibinputConfigV1 _ (RiverLibinputConfigV1LibinputDevice d) -> do
    modify $ \s -> s { libinputDevices = M.insert d (LiDev 0 False False False False 0 False) (libinputDevices s) }
    done
  EvRiverLibinputConfigV1 {} -> done
  EvRiverLibinputDeviceV1 d e -> case e of
    RiverLibinputDeviceV1TapSupport n -> updLi d (\x -> x { ldTapFingers = fromIntegral n }) >> done
    RiverLibinputDeviceV1NaturalScrollSupport n -> updLi d (\x -> x { ldNatural = n /= 0 }) >> done
    RiverLibinputDeviceV1DwtSupport n -> updLi d (\x -> x { ldDwt = n /= 0 }) >> done
    RiverLibinputDeviceV1LeftHandedSupport n -> updLi d (\x -> x { ldLeftHanded = n /= 0 }) >> done
    RiverLibinputDeviceV1MiddleEmulationSupport n -> updLi d (\x -> x { ldMiddle = n /= 0 }) >> done
    RiverLibinputDeviceV1AccelProfilesSupport p -> updLi d (\x -> x { ldAccelProfiles = p }) >> done
    RiverLibinputDeviceV1Done -> applyPointer d >> done
    RiverLibinputDeviceV1Removed -> do
      withConn (\c -> riverLibinputDeviceV1Destroy c d)
      modify $ \s -> s { libinputDevices = M.delete d (libinputDevices s) }
      done
    _ -> done
  -- Results of libinput settings: the object is gone after its one event.
  EvRiverLibinputResultV1 r e -> do
    withConn (\c -> forgetObject c r)
    case e of
      RiverLibinputResultV1Success -> pure ()
      RiverLibinputResultV1Unsupported -> logErr "a libinput setting is not supported by the device"
      RiverLibinputResultV1Invalid -> logErr "a libinput setting was rejected as invalid"
    done
  _ -> pure False
  where
    done = pure True
    updDev d f = modify $ \s -> s { inputDevices = M.adjust f d (inputDevices s) }
    updKb k f = modify $ \s -> s { xkbKeyboards = M.adjust f k (xkbKeyboards s) }
    updLi d f = modify $ \s -> s { libinputDevices = M.adjust f d (libinputDevices s) }

utf8 :: BC.ByteString -> String
utf8 = T.unpack . TE.decodeUtf8Lenient

------------------------------------------------------------------------------
-- Keyboards

-- | Compile the configured keymap and hand it to river (once, at startup).
-- Keyboards get it as soon as river accepts it.
setupKeymap :: X ()
setupKeymap = do
  kc <- asks (keyboard . input . config)
  xcfg <- asks (gXkbConfig . globals)
  let wanted = any (not . null) [kbLayout kc, kbVariant kc, kbOptions kc, kbModel kc]
  forM_ xcfg $ \x -> when wanted $ do
    fd <- io $ withCString "evdev" $ \r -> withCString (kbModel kc) $ \m ->
      withCString (kbLayout kc) $ \l -> withCString (kbVariant kc) $ \v ->
        withCString (kbOptions kc) $ \o -> c_keymap_fd r m l v o
    if fd < 0
      then do
        logErr ("cannot compile the keymap (layout " ++ show (kbLayout kc) ++ ", variant "
                ++ show (kbVariant kc) ++ ", options " ++ show (kbOptions kc) ++ ")")
        modify $ \s -> s { keymapState = KeymapFailed }
      else do
        m <- withConn $ \c -> riverXkbConfigV1CreateKeymap c x (Fd fd) riverXkbConfigV1KeymapFormatTextV1
        io (closeFd (Fd fd))   -- the connection keeps its own duplicate until sent
        modify $ \s -> s { keymapState = KeymapPending m }

applyKeymap :: ObjectId -> X ()
applyKeymap k = do
  st <- gets keymapState
  kb <- gets (M.lookup k . xkbKeyboards)
  case (st, kb) of
    (KeymapReady m, Just x) | not (xkKeymapSet x) -> do
      withConn (\c -> riverXkbKeyboardV1SetKeymap c k m)
      modify $ \s -> s { xkbKeyboards = M.adjust (\y -> y { xkKeymapSet = True }) k (xkbKeyboards s) }
    _ -> pure ()

applyNumlock :: ObjectId -> X ()
applyNumlock k = do
  want <- asks (kbNumlock . keyboard . input . config)
  kb <- gets (M.lookup k . xkbKeyboards)
  case (want, kb) of
    (Just on, Just x) | not (xkNumlockSet x) -> do
      withConn $ \c -> (if on then riverXkbKeyboardV1NumlockEnable else riverXkbKeyboardV1NumlockDisable) c k
      modify $ \s -> s { xkbKeyboards = M.adjust (\y -> y { xkNumlockSet = True }) k (xkbKeyboards s) }
    _ -> pure ()

-- | Switch every keyboard to the next of the configured layouts
-- (@kbLayout = "us,es"@).
nextKeyboardLayout :: X ()
nextKeyboardLayout = do
  n <- layoutCount
  kbs <- gets (M.toList . xkbKeyboards)
  when (n > 1) $ forM_ kbs $ \(k, x) ->
    withConn (\c -> riverXkbKeyboardV1SetLayoutByIndex c k (fromIntegral ((fst (xkLayout x) + 1) `mod` n)))

-- | Switch every keyboard to the layout with this index (0 = the first one).
setKeyboardLayout :: Int -> X ()
setKeyboardLayout i = do
  kbs <- gets (M.keys . xkbKeyboards)
  forM_ kbs $ \k -> withConn (\c -> riverXkbKeyboardV1SetLayoutByIndex c k (fromIntegral i))

-- | Name of the active layout (as xkb reports it, e.g. "Spanish"), if known.
currentKeyboardLayout :: X (Maybe String)
currentKeyboardLayout = gets $ \s ->
  listToMaybe [ n | x <- M.elems (xkbKeyboards s), let n = snd (xkLayout x), not (null n) ]

layoutCount :: X Int
layoutCount = do
  l <- asks (kbLayout . keyboard . input . config)
  pure (if null l then 1 else length (filter (== ',') l) + 1)

------------------------------------------------------------------------------
-- Key repeat

configureDevice :: ObjectId -> X ()
configureDevice d = do
  dev <- gets (M.lookup d . inputDevices)
  rep <- asks (kbRepeat . keyboard . input . config)
  forM_ dev $ \x -> unless (idConfigured x) $ do
    case (idType x, rep) of
      (Just 0, Just (rate, delay)) ->
        withConn (\c -> riverInputDeviceV1SetRepeatInfo c d (fromIntegral rate) (fromIntegral delay))
      _ -> pure ()
    modify $ \s -> s { inputDevices = M.adjust (\y -> y { idConfigured = True }) d (inputDevices s) }

------------------------------------------------------------------------------
-- Touchpads and mice

-- | Apply the touchpad or mouse settings once the device has described
-- itself. Touchpads are the devices with tap-to-click support.
applyPointer :: ObjectId -> X ()
applyPointer d = do
  li <- gets (M.lookup d . libinputDevices)
  ic <- asks (input . config)
  forM_ li $ \x -> unless (ldApplied x) $ do
    let isTouchpad = ldTapFingers x > 0
        pc = if isTouchpad then touchpad ic else mouse ic
        flag b = if b then 1 else 0
    c <- asks conn
    io $ do
      when isTouchpad $ forM_ (tapToClick pc) $ \b ->
        riverLibinputDeviceV1SetTap c d (flag b)
      when (ldNatural x) $ forM_ (naturalScroll pc) $ \b ->
        riverLibinputDeviceV1SetNaturalScroll c d (flag b)
      when (ldDwt x) $ forM_ (disableWhileTyping pc) $ \b ->
        riverLibinputDeviceV1SetDwt c d (flag b)
      when (ldLeftHanded x) $ forM_ (leftHanded pc) $ \b ->
        riverLibinputDeviceV1SetLeftHanded c d (flag b)
      when (ldMiddle x) $ forM_ (middleEmulation pc) $ \b ->
        riverLibinputDeviceV1SetMiddleEmulation c d (flag b)
      forM_ (accelProfile pc) $ \p -> do
        let (bit, val) = case p of
              AccelFlat -> (riverLibinputDeviceV1AccelProfilesFlat, riverLibinputDeviceV1AccelProfileFlat)
              AccelAdaptive -> (riverLibinputDeviceV1AccelProfilesAdaptive, riverLibinputDeviceV1AccelProfileAdaptive)
        when (ldAccelProfiles x `mod` (bit * 2) >= bit) $
          () <$ riverLibinputDeviceV1SetAccelProfile c d val
      -- the speed travels as a native double in a wl_array
      forM_ (accelSpeed pc) $ \sp ->
        riverLibinputDeviceV1SetAccelSpeed c d (BL.toStrict (BB.toLazyByteString (BB.doubleLE (max (-1) (min 1 sp)))))
    modify $ \s -> s { libinputDevices = M.adjust (\y -> y { ldApplied = True }) d (libinputDevices s) }
