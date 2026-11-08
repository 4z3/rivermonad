-- | Start programs once per session, not on every restart, as in
-- @XMonad.Util.SpawnOnce@.
--
-- > startupHook = do
-- >   spawnOnce "waybar"
-- >   spawnOnce "mako"
module Rivermonad.Util.SpawnOnce
  ( spawnOnce
  ) where

import Control.Monad (unless)
import qualified Data.Set as S

import Rivermonad.Core
import Rivermonad.Operations (spawn)
import qualified Rivermonad.Util.ExtensibleState as XS

newtype SpawnOnce = SpawnOnce { unSpawnOnce :: S.Set String }
  deriving (Read, Show)

instance ExtensionClass SpawnOnce where
  initialValue = SpawnOnce S.empty
  extensionType = PersistentExtension

-- | Spawn the command unless it was already spawned in this session.
spawnOnce :: String -> X ()
spawnOnce cmd = do
  done <- XS.gets (S.member cmd . unSpawnOnce)
  unless done $ do
    spawn cmd
    XS.modify (SpawnOnce . S.insert cmd . unSpawnOnce)
