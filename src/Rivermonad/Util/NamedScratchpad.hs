-- | Toggleable "scratchpad" windows, as in @XMonad.Util.NamedScratchpad@.
--
-- > scratchpads =
-- >   [ NS "term" "foot --app-id scratch-term" (appId =? "scratch-term")
-- >        (customFloating (W.RationalRect 0.1 0.1 0.8 0.6))
-- >   ]
-- >
-- > main = rivermonad def
-- >   { manageHook = namedScratchpadManageHook scratchpads
-- >   } `additionalKeysP` [("M-`", namedScratchpadAction scratchpads "term")]
--
-- Hidden scratchpads live on the workspace 'scratchpadWorkspaceTag', which
-- status bars and "Rivermonad.Actions.CycleWS" skip.
module Rivermonad.Util.NamedScratchpad
  ( NamedScratchpad (..)
  , NamedScratchpads
  , namedScratchpadAction
  , namedScratchpadManageHook
  , scratchpadWorkspaceTag
  , customFloating
  , nonFloating
  , defaultFloating
  ) where

import Control.Monad (filterM, unless)
import Data.List (find)

import Rivermonad.Core
import Rivermonad.Hooks.ManageHelpers (doRectFloat)
import Rivermonad.ManageHook
import Rivermonad.Operations (spawn, windows)
import qualified Rivermonad.StackSet as W

data NamedScratchpad = NS
  { name  :: String      -- ^ used with 'namedScratchpadAction'
  , cmd   :: String      -- ^ command that starts it
  , query :: Query Bool  -- ^ recognises its window
  , hook  :: ManageHook  -- ^ applied when its window appears
  }

type NamedScratchpads = [NamedScratchpad]

scratchpadWorkspaceTag :: WorkspaceId
scratchpadWorkspaceTag = "NSP"

customFloating :: W.RationalRect -> ManageHook
customFloating = doRectFloat

nonFloating :: ManageHook
nonFloating = idHook

defaultFloating :: ManageHook
defaultFloating = doFloat

-- | Show the scratchpad on the current workspace, hide it if it is already
-- there, or start it if it is not running.
namedScratchpadAction :: NamedScratchpads -> String -> X ()
namedScratchpadAction confs n = case find ((n ==) . name) confs of
  Nothing -> pure ()
  Just conf -> do
    ws <- gets windowset
    here <- filterM (runQuery (query conf)) (W.index ws)
    case here of
      (w : _) -> do
        ensureScratchpadWorkspace
        windows (W.shiftWin scratchpadWorkspaceTag w)
      [] -> do
        elsewhere <- filterM (runQuery (query conf)) (W.allWindows ws)
        case elsewhere of
          (w : _) -> windows $ \s -> W.focusWindow w (W.shiftWin (W.currentTag s) w s)
          [] -> spawn (cmd conf)

namedScratchpadManageHook :: NamedScratchpads -> ManageHook
namedScratchpadManageHook confs = composeAll [ query c --> hook c | c <- confs ]

-- | The hidden workspace is created on demand.
ensureScratchpadWorkspace :: X ()
ensureScratchpadWorkspace = do
  ws <- gets windowset
  unless (W.tagMember scratchpadWorkspaceTag ws) $ do
    let l = W.layout (W.workspace (W.current ws))
    windows $ \s -> s { W.hidden = W.Workspace scratchpadWorkspaceTag l Nothing : W.hidden s }
