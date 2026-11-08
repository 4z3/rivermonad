-- | Choose where new windows enter the stack, as in
-- @XMonad.Hooks.InsertPosition@.
--
-- > manageHook = insertPosition Below Newer <> myManageHook
module Rivermonad.Hooks.InsertPosition
  ( Position (..)
  , Focus (..)
  , insertPosition
  ) where


import Rivermonad.Core
import Rivermonad.ManageHook (doF)
import qualified Rivermonad.StackSet as W

data Position = Master | End | Above | Below deriving (Show, Eq)

-- | Whether the new window ('Newer') or the previously focused one ('Older')
-- keeps the focus.
data Focus = Newer | Older deriving (Show, Eq)

insertPosition :: Position -> Focus -> ManageHook
insertPosition pos foc = ask >>= \w -> doF (place w)
  where
    place w ws = case W.findTag w ws of
      Nothing -> ws
      Just t -> W.view (W.currentTag ws) . updateFocus w . ins w . W.delete' w . W.view t $ ws
    -- Insert, then give the focus back to whatever had it.
    ins w ws = maybe id W.focusWindow (W.peek ws) (insert w ws)
    insert w = case pos of
      Master -> W.insertUp w . W.focusMaster
      End    -> insertDown w . W.modify' focusLast
      Above  -> W.insertUp w
      Below  -> insertDown w
    updateFocus w = case foc of
      Newer -> W.focusWindow w
      Older -> id
    insertDown w = W.swapDown . W.insertUp w
    focusLast st = case reverse (W.integrate st) of
      (l : rest) -> W.Stack l rest []
      [] -> st
