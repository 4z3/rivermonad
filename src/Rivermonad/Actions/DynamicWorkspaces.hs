-- | Workspaces that come and go at runtime, as in
-- @XMonad.Actions.DynamicWorkspaces@.
--
-- The configured 'workspaces' are only the initial list. The current list
-- and its order live in the window manager's state ('workspaceTags'),
-- survive a restart, and are what bars and pagers see over IPC.
--
-- > , ("M-<Delete>", removeEmptyWorkspace)
-- > , ("M-n",        addWorkspace "scratch")
--
-- The same operations are available on the IPC socket (@AddWorkspace@,
-- @RemoveWorkspace@, @RenameWorkspace@, @SetWorkspaces@), so an external
-- pager can manage the list.
module Rivermonad.Actions.DynamicWorkspaces
  ( -- * Actions
    addWorkspace
  , addHiddenWorkspace
  , removeEmptyWorkspace
  , removeEmptyWorkspaceByTag
  , renameWorkspace
  , renameWorkspaceByName
  , setWorkspaces
  , workspaceTags
    -- * Pure parts
  , addHidden
  , removeEmpty
  , renameTag'
  , Plan (..)
  , plan
  ) where

import Control.Monad (forM_, unless, when)
import Data.List ((\\))
import qualified Data.Set as S

import Rivermonad.Core
import Rivermonad.Operations (windows)
import qualified Rivermonad.StackSet as W

-- | The user-visible workspaces, in order. Internal workspaces (those some
-- module added to the window set without listing them, like the scratchpad
-- one) are left out.
workspaceTags :: X [WorkspaceId]
workspaceTags = do
  ws <- gets windowset
  order <- gets workspaceOrder
  pure (filter (`W.tagMember` ws) order)

-- | Create a workspace and show it.
addWorkspace :: WorkspaceId -> X ()
addWorkspace t = do
  addHiddenWorkspace t
  windows (W.greedyView t)

-- | Create a workspace without showing it. Does nothing if it exists.
addHiddenWorkspace :: WorkspaceId -> X ()
addHiddenWorkspace t = unless (null t) $ do
  l <- asks (layoutHook . config)
  windows (addHidden l t)
  modify $ \s -> s { workspaceOrder = workspaceOrder s ++ [ t | t `notElem` workspaceOrder s ] }

-- | Remove the current workspace if it has no windows and another
-- workspace can take its place.
removeEmptyWorkspace :: X ()
removeEmptyWorkspace = gets (W.currentTag . windowset) >>= removeEmptyWorkspaceByTag

-- | Remove a workspace if it has no windows. A workspace that is on a
-- screen is replaced there by a hidden one; if there is none, it stays.
removeEmptyWorkspaceByTag :: WorkspaceId -> X ()
removeEmptyWorkspaceByTag t = do
  ws <- gets windowset
  forM_ (removeEmpty t ws) $ \ws' -> do
    windows (const ws')
    modify $ \s -> s { workspaceOrder = filter (/= t) (workspaceOrder s) }

-- | Rename the current workspace.
renameWorkspaceByName :: WorkspaceId -> X ()
renameWorkspaceByName new = do
  cur <- gets (W.currentTag . windowset)
  renameWorkspace cur new

-- | Rename a workspace. Does nothing if the new name is taken or empty.
renameWorkspace :: WorkspaceId -> WorkspaceId -> X ()
renameWorkspace old new = do
  ws <- gets windowset
  when (not (null new) && W.tagMember old ws && not (W.tagMember new ws)) $ do
    windows (renameTag' old new)
    modify $ \s -> s { workspaceOrder = map (\t -> if t == old then new else t) (workspaceOrder s) }

-- | Make the given names the workspace list, in that order.
--
-- New names are created and missing names are removed if they are empty.
-- When a name disappears from a position where a new one appears, that is
-- a rename, so its windows stay on it. Workspaces that cannot be removed
-- (they still have windows) are kept, after the requested ones.
setWorkspaces :: [WorkspaceId] -> X ()
setWorkspaces wanted0 = do
  let wanted = dedup (filter (not . null) wanted0)
  old <- workspaceTags
  let p = plan old wanted
  mapM_ (uncurry renameWorkspace) (planRename p)
  mapM_ addHiddenWorkspace (planCreate p)
  mapM_ removeEmptyWorkspaceByTag (planRemove p)
  now <- workspaceTags
  modify $ \s -> s { workspaceOrder = filter (`elem` now) wanted ++ (now \\ wanted) }
  where
    dedup = go S.empty
      where go _ [] = []
            go seen (x : xs) | S.member x seen = go seen xs
                             | otherwise = x : go (S.insert x seen) xs

------------------------------------------------------------------------------
-- Pure parts

-- | Append an empty workspace to the hidden ones, unless the tag exists.
addHidden :: Eq i => l -> i -> W.StackSet i l a s sd -> W.StackSet i l a s sd
addHidden l t ws
  | W.tagMember t ws = ws
  | otherwise = ws { W.hidden = W.hidden ws ++ [W.Workspace t l Nothing] }

-- | Remove an empty workspace. 'Nothing' if it does not exist, has
-- windows, or is on a screen while no hidden workspace can replace it.
removeEmpty :: (Eq i, Eq s) => i -> W.StackSet i l a s sd -> Maybe (W.StackSet i l a s sd)
removeEmpty t ws = case [ w | w <- W.workspaces ws, W.tag w == t ] of
  [w] | null (W.integrate' (W.stack w)) -> dropIt
  _ -> Nothing
  where
    isT w = W.tag w == t
    dropIt
      | any isT (W.hidden ws) = Just ws { W.hidden = filter (not . isT) (W.hidden ws) }
      | otherwise = case W.hidden ws of
          [] -> Nothing
          (h : hs) ->
            let swap scr = if isT (W.workspace scr) then scr { W.workspace = h } else scr
            in Just ws { W.current = swap (W.current ws), W.visible = map swap (W.visible ws), W.hidden = hs }

-- | Rename a tag (the caller checks that the new one is free).
renameTag' :: Eq i => i -> i -> W.StackSet i l a s sd -> W.StackSet i l a s sd
renameTag' old new = W.mapWorkspace (\w -> if W.tag w == old then w { W.tag = new } else w)

-- | What 'setWorkspaces' does to get from one list of names to another.
data Plan = Plan
  { planRename :: [(WorkspaceId, WorkspaceId)]
  , planCreate :: [WorkspaceId]
  , planRemove :: [WorkspaceId]
  } deriving (Eq, Show)

-- | Walk both lists together. A name that is going away, met at the same
-- step as a name that is new, is renamed to it; other names going away are
-- removed and other new names are created.
plan :: [WorkspaceId] -> [WorkspaceId] -> Plan
plan old new = Plan renames (filter (`S.notMember` renamedTo) (S.toAscList created))
                            (filter (`S.notMember` renamedFrom) (S.toAscList deleted))
  where
    oldSet = S.fromList old
    newSet = S.fromList new
    deleted = S.difference oldSet newSet
    created = S.difference newSet oldSet
    renames = go old new
    renamedFrom = S.fromList (map fst renames)
    renamedTo = S.fromList (map snd renames)
    go os@(o : ot) ns@(n : nt)
      | o == n = go ot nt
      | gone && fresh = (o, n) : go ot nt
      | gone = go ot ns
      | fresh = go os nt
      | otherwise = go ot nt
      where gone = S.member o deleted
            fresh = S.member n created
    go _ _ = []
