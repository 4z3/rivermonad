-- | Everything a configuration file needs, like @import XMonad@.
--
-- > import Rivermonad
-- > import qualified Rivermonad.StackSet as W
-- >
-- > main :: IO ()
-- > main = rivermonad def { terminal = "foot", modMask = mod4Mask }
module Rivermonad
  ( module Rivermonad.Core
  , module Rivermonad.Config
  , module Rivermonad.Keysym
  , module Rivermonad.Layout
  , module Rivermonad.ManageHook
  , module Rivermonad.Operations
  , rivermonad
  , (.|.)
  , M.Map
  ) where

import Data.Bits ((.|.))
import qualified Data.Map.Strict as M

import Rivermonad.Config
import Rivermonad.Core
import Rivermonad.Keysym
import Rivermonad.Layout
import Rivermonad.Main (rivermonad)
import Rivermonad.ManageHook
import Rivermonad.Operations
