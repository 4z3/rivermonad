-- | Status bar output, the Wayland counterpart of @XMonad.Hooks.StatusBar@ /
-- @DynamicLog@. rivermonad serves one JSON line per change on a Unix socket;
-- @rivermonad --status@ prints them, which is what a waybar @custom@ module
-- expects:
--
-- > -- rivermonad.hs
-- > main = rivermonad (withStatusBar def myConfig)
--
-- > // ~/.config/waybar/config.jsonc
-- > "modules-left": ["custom/rivermonad"],
-- > "custom/rivermonad": {
-- >   "exec": "rivermonad --status",
-- >   "return-type": "json",
-- >   "escape": false
-- > }
--
-- The @text@ field uses Pango markup; window titles and workspace names are
-- escaped before the 'PP' functions add their own markup.
module Rivermonad.Hooks.StatusBar
  ( PP (..)
  , withStatusBar
  , statusLogHook
  , startStatusServer
  , dynamicLogString
  , wrap
  , pad
  , shorten
  , pangoEscape
  , pangoColor
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import qualified Control.Exception as E
import Control.Monad (filterM, forM_, forever, unless, void)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Char (ord)
import Data.IORef
import Data.List (elemIndex, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, isJust)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import System.Directory (removeFile)
import Text.Printf (printf)

import Rivermonad.Core
import Rivermonad.Status (statusSocketPath)
import qualified Rivermonad.StackSet as W
import qualified Rivermonad.Util.ExtensibleState as XS
import Rivermonad.Util.NamedScratchpad (scratchpadWorkspaceTag)

-- | How to render the status line ("pretty printer"), as in xmonad.
data PP = PP
  { ppCurrent         :: WorkspaceId -> String
  , ppVisible         :: WorkspaceId -> String  -- ^ shown on another screen
  , ppHidden          :: WorkspaceId -> String  -- ^ has windows
  , ppHiddenNoWindows :: WorkspaceId -> String  -- ^ empty
  , ppWsSep           :: String
  , ppSep             :: String
  , ppTitle           :: String -> String
  , ppTitleSanitizer  :: String -> String       -- ^ applied before 'ppTitle'
  , ppLayout          :: String -> String
  , ppOrder           :: [String] -> [String]   -- ^ [workspaces, layout, title, extras...]
  , ppSort            :: X ([WindowSpace] -> [WindowSpace])
  , ppExtras          :: [X (Maybe String)]
  }

instance Default PP where
  def = PP
    { ppCurrent = wrap "<b>[" "]</b>"
    , ppVisible = wrap "(" ")"
    , ppHidden = id
    , ppHiddenNoWindows = const ""
    , ppWsSep = " "
    , ppSep = " : "
    , ppTitle = id
    , ppTitleSanitizer = pangoEscape . shorten 80
    , ppLayout = id
    , ppOrder = id
    , ppSort = configOrder
    , ppExtras = []
    }

-- | Workspaces in the order of the configuration, scratchpads left out.
configOrder :: X ([WindowSpace] -> [WindowSpace])
configOrder = do
  tags <- asks (workspaces . config)
  let idx t = fromMaybe maxBound (elemIndex t tags)
  pure (sortOn (idx . W.tag) . filter ((/= scratchpadWorkspaceTag) . W.tag))

wrap :: String -> String -> String -> String
wrap l r m = if null m then "" else l ++ m ++ r

pad :: String -> String
pad = wrap " " " "

shorten :: Int -> String -> String
shorten n s = if length s > n then take (n - 1) s ++ "…" else s

pangoEscape :: String -> String
pangoEscape = concatMap esc
  where
    esc '&' = "&amp;"
    esc '<' = "&lt;"
    esc '>' = "&gt;"
    esc '"' = "&quot;"
    esc '\'' = "&#39;"
    esc c = [c]

-- | Foreground colour, e.g. @pangoColor "#88c0d0"@.
pangoColor :: String -> String -> String
pangoColor fg = wrap ("<span foreground=\"" ++ fg ++ "\">") "</span>"

-- | Add the status server and its log hook to a configuration.
withStatusBar :: PP -> XConfig -> XConfig
withStatusBar pp conf = conf
  { startupHook = startupHook conf >> startStatusServer
  , logHook = logHook conf >> statusLogHook pp
  }

dynamicLogString :: PP -> X String
dynamicLogString pp = do
  ws <- gets windowset
  infos <- gets windowInfo
  sorter <- ppSort pp
  let cur = W.currentTag ws
      visibleTags = map (W.tag . W.workspace) (W.visible ws)
      fmt wsp
        | W.tag wsp == cur = ppCurrent pp t
        | W.tag wsp `elem` visibleTags = ppVisible pp t
        | isJust (W.stack wsp) = ppHidden pp t
        | otherwise = ppHiddenNoWindows pp t
        where t = pangoEscape (W.tag wsp)
      wsStr = unwordsWith (ppWsSep pp) (filter (not . null) (map fmt (sorter (W.workspaces ws))))
      layoutStr = ppLayout pp (pangoEscape (description (W.layout (W.workspace (W.current ws)))))
      title = maybe "" wiTitle (W.peek ws >>= \w -> M.lookup w infos)
      titleStr = ppTitle pp (ppTitleSanitizer pp title)
  extras <- catMaybes <$> mapM (userCodeDef Nothing) (ppExtras pp)
  pure (unwordsWith (ppSep pp) (filter (not . null) (ppOrder pp ([wsStr, layoutStr, titleStr] ++ extras))))
  where
    unwordsWith sep = foldr1' (\a b -> a ++ sep ++ b)
    foldr1' _ [] = ""
    foldr1' f xs = foldr1 f xs

------------------------------------------------------------------------------
-- The socket server

data Server = Server
  { srvClients :: MVar [NS.Socket]
  , srvLast    :: IORef BC.ByteString
  }

newtype StatusServer = StatusServer (Maybe Server)

instance ExtensionClass StatusServer where
  initialValue = StatusServer Nothing

-- | Start listening (idempotent). Called from the startup hook.
startStatusServer :: X ()
startStatusServer = do
  StatusServer running <- XS.get
  unless (isJust running) $ do
    r <- io $ E.try $ do
      path <- statusSocketPath
      _ <- E.try (removeFile path) :: IO (Either E.IOException ())
      sock <- NS.socket NS.AF_UNIX NS.Stream NS.defaultProtocol
      NS.withFdSocket sock NS.setCloseOnExecIfNeeded
      NS.bind sock (NS.SockAddrUnix path)
      NS.listen sock 8
      srv <- Server <$> newMVar [] <*> newIORef BC.empty
      void $ forkIO $ forever $ do
        (client, _) <- NS.accept sock
        NS.withFdSocket client NS.setCloseOnExecIfNeeded
        modifyMVar_ (srvClients srv) $ \cs -> do
          lastLine <- readIORef (srvLast srv)
          ok <- sendTo client lastLine
          pure (if ok then client : cs else cs)
      pure srv
    case r of
      Right srv -> XS.put (StatusServer (Just srv))
      Left e -> io (putStrLn ("rivermonad: status server: " ++ show (e :: E.SomeException)))

-- | Publish the current state (only when it changed).
statusLogHook :: PP -> X ()
statusLogHook pp = do
  StatusServer running <- XS.get
  forM_ running $ \srv -> do
    text <- dynamicLogString pp
    ws <- gets windowset
    infos <- gets windowInfo
    let title = maybe "" wiTitle (W.peek ws >>= \w -> M.lookup w infos)
        lay = description (W.layout (W.workspace (W.current ws)))
        line = TE.encodeUtf8 $ T.pack $ "{\"text\":" ++ jsonString text
                  ++ ",\"tooltip\":" ++ jsonString (pangoEscape title)
                  ++ ",\"alt\":" ++ jsonString (W.currentTag ws)
                  ++ ",\"class\":" ++ jsonString (map (\c -> if c == ' ' then '-' else c) lay)
                  ++ "}\n"
    io $ do
      prev <- readIORef (srvLast srv)
      unless (line == prev) $ do
        writeIORef (srvLast srv) line
        modifyMVar_ (srvClients srv) (filterM (`sendTo` line))

sendTo :: NS.Socket -> BC.ByteString -> IO Bool
sendTo s bytes
  | BC.null bytes = pure True
  | otherwise = do
      r <- E.try (NSB.sendAll s bytes) :: IO (Either E.IOException ())
      case r of
        Right () -> pure True
        Left _ -> NS.close s >> pure False

jsonString :: String -> String
jsonString s = "\"" ++ concatMap esc s ++ "\""
  where
    esc '"' = "\\\""
    esc '\\' = "\\\\"
    esc '\n' = "\\n"
    esc '\t' = "\\t"
    esc c | ord c < 0x20 = printf "\\u%04x" (ord c)
          | otherwise = [c]
