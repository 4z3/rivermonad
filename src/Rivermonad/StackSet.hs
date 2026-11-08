-- | The pure window model, API-compatible with @XMonad.StackSet@.
--
-- A 'StackSet' is a set of virtual workspaces, some of them shown on a
-- physical screen. Each workspace holds a zipper ('Stack') over its windows,
-- so there is always exactly one focused window per non-empty workspace.
module Rivermonad.StackSet
  ( StackSet (..), Screen (..), Workspace (..), Stack (..), RationalRect (..)
  , new, view, greedyView, lookupWorkspace
  , screens, workspaces, allWindows, currentTag
  , peek, index, integrate, integrate', differentiate
  , focusUp, focusDown, focusUp', focusDown', focusMaster, focusWindow
  , swapUp, swapDown, swapMaster, shiftMaster, modify, modify'
  , member, findTag, tagMember, renameTag, mapWorkspace, mapLayout
  , insertUp, delete, delete', filter
  , float, sink, shift, shiftWin, onWorkspace
  ) where

import Prelude hiding (filter)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, listToMaybe)

data StackSet i l a sid sd = StackSet
  { current  :: !(Screen i l a sid sd)    -- ^ focused screen
  , visible  :: [Screen i l a sid sd]     -- ^ other visible screens
  , hidden   :: [Workspace i l a]         -- ^ workspaces not on any screen
  , floating :: M.Map a RationalRect      -- ^ floating windows, relative to their screen
  } deriving (Show, Eq)

data Screen i l a sid sd = Screen
  { workspace    :: !(Workspace i l a)
  , screen       :: !sid
  , screenDetail :: !sd
  } deriving (Show, Eq)

data Workspace i l a = Workspace
  { tag    :: !i
  , layout :: l
  , stack  :: Maybe (Stack a)
  } deriving (Show, Eq)

-- | Position and size as fractions of the screen.
data RationalRect = RationalRect !Rational !Rational !Rational !Rational
  deriving (Show, Read, Eq)

data Stack a = Stack
  { focus :: !a
  , up    :: [a]   -- ^ windows above the focus, nearest first
  , down  :: [a]
  } deriving (Show, Read, Eq)

new :: Integral s => l -> [i] -> [sd] -> StackSet i l a s sd
new l wids m
  | not (null wids), not (null m), length m <= length wids =
      case [ Screen (ws i) s sd | (i, s, sd) <- zip3 seen [0 ..] m ] of
        (cur : visi) -> StackSet cur visi (map ws unseen) M.empty
        []           -> error "StackSet.new: impossible"
  | otherwise = error "StackSet.new: need at least one workspace per screen"
  where
    (seen, unseen) = splitAt (length m) wids
    ws i = Workspace i l Nothing

-- | Focus a workspace. If it is visible on another screen, focus that screen.
view :: (Eq s, Eq i) => i -> StackSet i l a s sd -> StackSet i l a s sd
view i s
  | i == currentTag s = s
  | Just x <- L.find ((i ==) . tag . workspace) (visible s) =
      s { current = x
        , visible = current s : L.deleteBy (\a b -> screen a == screen b) x (visible s) }
  | Just x <- L.find ((i ==) . tag) (hidden s) =
      s { current = (current s) { workspace = x }
        , hidden = workspace (current s) : L.deleteBy (\a b -> tag a == tag b) x (hidden s) }
  | otherwise = s

-- | Like 'view', but if the workspace is visible on another screen, swap it
-- onto the current one instead of moving the focus.
greedyView :: (Eq s, Eq i) => i -> StackSet i l a s sd -> StackSet i l a s sd
greedyView i ws
  | any wTag (hidden ws) = view i ws
  | Just s <- L.find (wTag . workspace) (visible ws) =
      ws { current = (current ws) { workspace = workspace s }
         , visible = s { workspace = workspace (current ws) }
                   : L.filter (not . wTag . workspace) (visible ws) }
  | otherwise = ws
  where wTag = (i ==) . tag

lookupWorkspace :: Eq s => s -> StackSet i l a s sd -> Maybe i
lookupWorkspace sc w = listToMaybe [ tag i | Screen i s _ <- current w : visible w, s == sc ]

with :: b -> (Stack a -> b) -> StackSet i l a s sd -> b
with dflt f = maybe dflt f . stack . workspace . current

-- | Apply a function to the current stack, with a default for empty ones.
modify :: Maybe (Stack a) -> (Stack a -> Maybe (Stack a)) -> StackSet i l a s sd -> StackSet i l a s sd
modify d f s = s { current = (current s)
                   { workspace = (workspace (current s)) { stack = with d f s } } }

modify' :: (Stack a -> Stack a) -> StackSet i l a s sd -> StackSet i l a s sd
modify' f = modify Nothing (Just . f)

peek :: StackSet i l a s sd -> Maybe a
peek = with Nothing (Just . focus)

integrate :: Stack a -> [a]
integrate (Stack x l r) = reverse l ++ x : r

integrate' :: Maybe (Stack a) -> [a]
integrate' = maybe [] integrate

differentiate :: [a] -> Maybe (Stack a)
differentiate []       = Nothing
differentiate (x : xs) = Just (Stack x [] xs)

-- | Keep the windows satisfying the predicate, trying to preserve the focus.
filter :: (a -> Bool) -> Stack a -> Maybe (Stack a)
filter p (Stack f ls rs) = case L.filter p (f : rs) of
  f' : rs' -> Just (Stack f' (L.filter p ls) rs')
  []       -> case L.filter p ls of
    f' : ls' -> Just (Stack f' ls' [])
    []       -> Nothing

index :: StackSet i l a s sd -> [a]
index = with [] integrate

focusUp, focusDown, swapUp, swapDown :: StackSet i l a s sd -> StackSet i l a s sd
focusUp   = modify' focusUp'
focusDown = modify' focusDown'
swapUp    = modify' swapUp'
swapDown  = modify' (reverseStack . swapUp' . reverseStack)

focusUp', focusDown', swapUp', reverseStack :: Stack a -> Stack a
focusUp' (Stack t (l : ls) rs) = Stack l ls (t : rs)
focusUp' (Stack t [] rs) = case reverse (t : rs) of
  x : xs -> Stack x xs []
  []     -> Stack t [] rs
focusDown' = reverseStack . focusUp' . reverseStack
swapUp' (Stack t (l : ls) rs) = Stack t ls (l : rs)
swapUp' (Stack t [] rs)       = Stack t (reverse rs) []
reverseStack (Stack t ls rs)  = Stack t rs ls

-- | Focus the given window, switching workspace (and screen) if needed.
focusWindow :: (Eq s, Eq a, Eq i) => a -> StackSet i l a s sd -> StackSet i l a s sd
focusWindow w s
  | Just w == peek s = s
  | otherwise = fromMaybe s $ do
      n <- findTag w s
      let s' = view n s
      pure $ modify' (rotateTo w) s'
  where
    rotateTo x st = case break (== x) (integrate st) of
      (before, _ : after) -> Stack x (reverse before) after
      _                   -> st

-- | Move the focused window to the master position, keeping order otherwise.
swapMaster :: StackSet i l a s sd -> StackSet i l a s sd
swapMaster = modify' $ \c -> case c of
  Stack t ls rs -> case reverse ls of
    x : xs -> Stack t [] (xs ++ x : rs)
    []     -> c

shiftMaster :: StackSet i l a s sd -> StackSet i l a s sd
shiftMaster = modify' $ \c -> case c of
  Stack t ls rs -> Stack t [] (reverse ls ++ rs)

focusMaster :: StackSet i l a s sd -> StackSet i l a s sd
focusMaster = modify' $ \c -> case c of
  Stack t ls rs -> case reverse ls of
    x : xs -> Stack x [] (xs ++ t : rs)
    []     -> c

screens :: StackSet i l a s sd -> [Screen i l a s sd]
screens s = current s : visible s

workspaces :: StackSet i l a s sd -> [Workspace i l a]
workspaces s = workspace (current s) : map workspace (visible s) ++ hidden s

allWindows :: Eq a => StackSet i l a s sd -> [a]
allWindows = L.nub . concatMap (integrate' . stack) . workspaces

currentTag :: StackSet i l a s sd -> i
currentTag = tag . workspace . current

tagMember :: Eq i => i -> StackSet i l a s sd -> Bool
tagMember t = elem t . map tag . workspaces

renameTag :: Eq i => i -> i -> StackSet i l a s sd -> StackSet i l a s sd
renameTag o n = mapWorkspace rename
  where rename w = if tag w == o then w { tag = n } else w

mapWorkspace :: (Workspace i l a -> Workspace i l a) -> StackSet i l a s sd -> StackSet i l a s sd
mapWorkspace f s = s { current = onScreen (current s)
                     , visible = map onScreen (visible s)
                     , hidden  = map f (hidden s) }
  where onScreen scr = scr { workspace = f (workspace scr) }

mapLayout :: (l -> l') -> StackSet i l a s sd -> StackSet i l' a s sd
mapLayout f (StackSet v vs hs m) = StackSet (onScreen v) (map onScreen vs) (map onWs hs) m
  where
    onScreen (Screen w s sd) = Screen (onWs w) s sd
    onWs (Workspace t l s) = Workspace t (f l) s

member :: Eq a => a -> StackSet i l a s sd -> Bool
member a s = isJust (findTag a s)

findTag :: Eq a => a -> StackSet i l a s sd -> Maybe i
findTag a s = listToMaybe [ tag w | w <- workspaces s, a `elem` integrate' (stack w) ]

-- | Insert a new window above the focus and focus it. No-op if already present.
insertUp :: Eq a => a -> StackSet i l a s sd -> StackSet i l a s sd
insertUp a s
  | member a s = s
  | otherwise = modify (Just (Stack a [] [])) (\(Stack t l r) -> Just (Stack a l (t : r))) s

-- | Remove a window everywhere, including the floating layer.
delete :: Ord a => a -> StackSet i l a s sd -> StackSet i l a s sd
delete w = sink w . delete' w

-- | Remove a window from the workspaces but keep its floating geometry.
delete' :: Eq a => a -> StackSet i l a s sd -> StackSet i l a s sd
delete' w = mapWorkspace (\ws -> ws { stack = stack ws >>= filter (/= w) })

float :: Ord a => a -> RationalRect -> StackSet i l a s sd -> StackSet i l a s sd
float w r s = s { floating = M.insert w r (floating s) }

sink :: Ord a => a -> StackSet i l a s sd -> StackSet i l a s sd
sink w s = s { floating = M.delete w (floating s) }

-- | Move the focused window to another workspace.
shift :: (Ord a, Eq s, Eq i) => i -> StackSet i l a s sd -> StackSet i l a s sd
shift n s = maybe s (\w -> shiftWin n w s) (peek s)

shiftWin :: (Ord a, Eq s, Eq i) => i -> a -> StackSet i l a s sd -> StackSet i l a s sd
shiftWin n w s = case findTag w s of
  Just from | n `tagMember` s, n /= from ->
    onWorkspace n (insertUp w) (onWorkspace from (delete' w) s)
  _ -> s

-- | Run a function as if the given workspace were the current one.
onWorkspace :: (Eq i, Eq s) => i -> (StackSet i l a s sd -> StackSet i l a s sd)
            -> StackSet i l a s sd -> StackSet i l a s sd
onWorkspace n f s = view (currentTag s) (f (view n s))
