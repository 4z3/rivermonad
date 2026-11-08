-- | The status channel between rivermonad and status bars: a Unix socket
-- where every connected client receives one line (JSON) per state change.
--
-- The server side lives in "Rivermonad.Hooks.StatusBar"; the client is
-- @rivermonad --status@, meant for waybar's @custom@ modules.
module Rivermonad.Status
  ( statusSocketPath
  , runStatusClient
  ) where

import Control.Concurrent (threadDelay)
import qualified Control.Exception as E
import Control.Monad (forever, unless)
import qualified Data.ByteString as B
import Data.Maybe (fromMaybe)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO (hFlush, stdout)

statusSocketPath :: IO FilePath
statusSocketPath = do
  dir <- fromMaybe "/tmp" <$> lookupEnv "XDG_RUNTIME_DIR"
  disp <- fromMaybe "wayland-0" <$> lookupEnv "WAYLAND_DISPLAY"
  pure (dir </> ("rivermonad-" ++ map (\c -> if c == '/' then '_' else c) disp ++ ".status"))

-- | Copy status lines to stdout forever, reconnecting when rivermonad
-- restarts (or is not running yet).
runStatusClient :: IO ()
runStatusClient = forever $ do
  path <- statusSocketPath
  r <- E.try $ E.bracket (NS.socket NS.AF_UNIX NS.Stream NS.defaultProtocol) NS.close $ \s -> do
    NS.connect s (NS.SockAddrUnix path)
    let loop = do
          chunk <- NSB.recv s 4096
          unless (B.null chunk) $ do
            B.putStr chunk
            hFlush stdout
            loop
    loop
  case r of
    Left e -> const (pure ()) (e :: E.IOException)
    Right () -> pure ()
  threadDelay 500000
