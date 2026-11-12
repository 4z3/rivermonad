{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Fixed gaps at the edges of the screen, one size per edge, as in
-- @XMonad.Layout.Gaps@. The wrapped layout gets what is left.
--
-- > layoutHook = Layout (gaps [(U, 18), (R, 23)] (Tall 1 (3/100) (1/2)))
-- > , ("M-g", sendMessage ToggleGaps)
--
-- For gaps /between/ windows see "Rivermonad.Layout.Spacing".
module Rivermonad.Layout.Gaps
  ( gaps
  , Gaps
  , GapSpec
  , Direction2D (..)
  , GapMessage (..)
  , applyGaps
  ) where

import Data.List (delete)

import Rivermonad.Core
import Rivermonad.Layout.LayoutModifier
import Rivermonad.StackSet (Workspace (..))

-- | The edges of a screen: up, down, right, left.
data Direction2D = U | D | R | L
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Pixels to leave free at some edges.
type GapSpec = [(Direction2D, Int)]

-- | The gap sizes and the edges whose gap is currently on.
data Gaps a = Gaps GapSpec [Direction2D]
  deriving (Show, Read)

data GapMessage
  = ToggleGaps                 -- ^ all gaps on or off
  | ToggleGap Direction2D      -- ^ one gap on or off
  | IncGap Int Direction2D     -- ^ make a gap larger
  | DecGap Int Direction2D     -- ^ make a gap smaller
  deriving (Show, Eq)

instance Message GapMessage

-- | Leave the given gaps around a layout.
gaps :: GapSpec -> l a -> ModifiedLayout Gaps l a
gaps spec = ModifiedLayout (Gaps spec (map fst spec))

instance LayoutModifier Gaps a where
  modifyLayout (Gaps spec on) (Workspace t l s) r =
    runLayout (Workspace t l s) (applyGaps [ g | g@(d, _) <- spec, d `elem` on ] r)

  pureMess (Gaps spec on) m = case fromMessage m of
    Just ToggleGaps
      | null on -> Just (Gaps spec (map fst spec))
      | otherwise -> Just (Gaps spec [])
    Just (ToggleGap d)
      | d `elem` on -> Just (Gaps spec (delete d on))
      | d `elem` map fst spec -> Just (Gaps spec (d : on))
      | otherwise -> Nothing
    Just (IncGap n d) -> Just (Gaps (resize n d) on)
    Just (DecGap n d) -> Just (Gaps (resize (negate n) d) on)
    Nothing -> Nothing
    where
      resize n d
        | d `elem` map fst spec = [ (e, if e == d then max 0 (g + n) else g) | (e, g) <- spec ]
        | otherwise = spec ++ [(d, max 0 n)]

-- | Shrink a rectangle by the gaps, never below one pixel.
applyGaps :: GapSpec -> Rectangle -> Rectangle
applyGaps spec (Rectangle x y w h) = Rectangle (x + l') (y + u') (max 1 (w - l' - r')) (max 1 (h - u' - d'))
  where
    size d = sum [ max 0 g | (e, g) <- spec, e == d ]
    -- Gaps wider than the screen are cut down, the left and top ones last.
    l' = min (size L) (max 0 (w - 1))
    r' = min (size R) (max 0 (w - 1 - l'))
    u' = min (size U) (max 0 (h - 1))
    d' = min (size D) (max 0 (h - 1 - u'))
