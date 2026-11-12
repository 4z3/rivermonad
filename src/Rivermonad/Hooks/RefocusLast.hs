{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Remember the window that had the focus before the current one, on
-- every workspace, and jump back to it, as in @XMonad.Hooks.RefocusLast@.
--
-- > layoutHook = Layout (refocusLastLayoutHook (Tall 1 (3/100) (1/2)))
-- > , ("M-l", toggleFocus)
--
-- Without the layout hook, put 'refocusLastLogHook' in the 'logHook'.
module Rivermonad.Hooks.RefocusLast
  ( refocusLastLayoutHook
  , refocusLastLogHook
  , toggleFocus
  , RefocusLastLayoutHook (..)
  , Recent (..)
  , recentsOn
  ) where

import Control.Monad (forM_, when)
import qualified Data.Map.Strict as M

import Rivermonad.Core
import Rivermonad.Layout.LayoutModifier
import Rivermonad.Operations (windows)
import Rivermonad.StackSet (Workspace (..))
import qualified Rivermonad.StackSet as W
import qualified Rivermonad.Util.ExtensibleState as XS

-- | The two windows a workspace focused most recently.
data Recent = Recent { previous :: !Window, current :: !Window }
  deriving (Show, Eq)

newtype RecentsMap = RecentsMap (M.Map WorkspaceId Recent)

instance ExtensionClass RecentsMap where
  initialValue = RecentsMap M.empty

data RefocusLastLayoutHook a = RefocusLastLayoutHook
  deriving (Show, Read)

-- | Record focus changes of the workspaces this layout is used on.
refocusLastLayoutHook :: l a -> ModifiedLayout RefocusLastLayoutHook l a
refocusLastLayoutHook = ModifiedLayout RefocusLastLayoutHook

instance LayoutModifier RefocusLastLayoutHook a where
  modifyLayout _ w@(Workspace t _ _) r = updateRecentsOn t >> runLayout w r

-- | Record focus changes of the workspaces on screen.
refocusLastLogHook :: X ()
refocusLastLogHook = do
  ws <- gets windowset
  mapM_ (updateRecentsOn . W.tag . W.workspace) (W.screens ws)

updateRecentsOn :: WorkspaceId -> X ()
updateRecentsOn t = do
  ws <- gets windowset
  let focused = [ W.focus st | wsp <- W.workspaces ws, W.tag wsp == t, Just st <- [W.stack wsp] ]
  forM_ focused $ \fw -> XS.modify $ \(RecentsMap m) -> RecentsMap $ case M.lookup t m of
    Just (Recent _ cur) | cur == fw -> m
                        | otherwise -> M.insert t (Recent cur fw) m
    Nothing -> M.insert t (Recent fw fw) m

-- | The recent windows of a workspace, if both are still on it.
recentsOn :: WorkspaceId -> X (Maybe Recent)
recentsOn t = do
  ws <- gets windowset
  RecentsMap m <- XS.get
  let here = concat [ W.integrate' (W.stack wsp) | wsp <- W.workspaces ws, W.tag wsp == t ]
  pure $ case M.lookup t m of
    Just r | previous r `elem` here, current r `elem` here -> Just r
    _ -> Nothing

-- | Focus the window that had the focus before, on the current workspace.
-- Doing it twice comes back.
toggleFocus :: X ()
toggleFocus = do
  t <- gets (W.currentTag . windowset)
  mr <- recentsOn t
  forM_ mr $ \(Recent lw cw) -> when (cw /= lw) (windows (W.focusWindow lw))
