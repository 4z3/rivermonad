{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Gaps between windows (and optionally at the screen edges), a simplified
-- @XMonad.Layout.Spacing@.
--
-- Two ways to give the size:
--
-- * 'spacing' / 'spacingWithEdge' / 'smartSpacing', as in xmonad: @n@ is
--   the margin /around each window/, so neighbours end up @2n@ apart.
-- * 'gaps' / 'gapsWithEdge' / 'smartGaps': @n@ is the exact gap between
--   windows (and to the screen edges), so any width works, odd ones too:
--   @gapsWithEdge 1@ leaves a single pixel.
--
-- > layoutHook = Layout (gapsWithEdge 1 (Tall 1 (3/100) (1/2)))
-- > , ("M-=", incSpacing 1), ("M--", incSpacing (-1)), ("M-g", toggleSpacing)
module Rivermonad.Layout.Spacing
  ( spacing
  , spacingWithEdge
  , smartSpacing
  , gaps
  , gapsWithEdge
  , smartGaps
  , Spacing (..)
  , SpacingMsg (..)
  , incSpacing
  , setSpacing
  , toggleSpacing
  ) where

import Rivermonad.Core
import Rivermonad.Layout.LayoutModifier
import Rivermonad.Operations (sendMessage)
import Rivermonad.StackSet (Stack, Workspace (..), integrate)

data Spacing a = Spacing
  { spacingSize    :: !Int    -- ^ see 'spacingExact'
  , spacingEdge    :: !Bool   -- ^ also leave a gap at the screen edges
  , spacingSmart   :: !Bool   -- ^ no gaps when there is a single window
  , spacingEnabled :: !Bool
  , spacingExact   :: !Bool   -- ^ True: size is the gap between windows;
                              --   False: the margin around each window (xmonad)
  } deriving (Show, Read)

-- | Gaps between windows only; @n@ around each window (xmonad style).
spacing :: Int -> l a -> ModifiedLayout Spacing l a
spacing n = ModifiedLayout (Spacing n False False True False)

-- | Like 'spacing', plus the same gap at the screen edges.
spacingWithEdge :: Int -> l a -> ModifiedLayout Spacing l a
spacingWithEdge n = ModifiedLayout (Spacing n True False True False)

-- | Like 'spacingWithEdge' but without gaps for a lone window.
smartSpacing :: Int -> l a -> ModifiedLayout Spacing l a
smartSpacing n = ModifiedLayout (Spacing n True True True False)

-- | Exactly @n@ pixels between windows.
gaps :: Int -> l a -> ModifiedLayout Spacing l a
gaps n = ModifiedLayout (Spacing n False False True True)

-- | Exactly @n@ pixels between windows and between windows and the screen
-- edges (or a bar).
gapsWithEdge :: Int -> l a -> ModifiedLayout Spacing l a
gapsWithEdge n = ModifiedLayout (Spacing n True False True True)

-- | Like 'gapsWithEdge' but without gaps for a lone window.
smartGaps :: Int -> l a -> ModifiedLayout Spacing l a
smartGaps n = ModifiedLayout (Spacing n True True True True)

data SpacingMsg = ModifySpacing (Int -> Int) | ToggleSpacing
instance Message SpacingMsg

incSpacing :: Int -> X ()
incSpacing d = sendMessage (ModifySpacing (max 0 . (+ d)))

setSpacing :: Int -> X ()
setSpacing n = sendMessage (ModifySpacing (const (max 0 n)))

toggleSpacing :: X ()
toggleSpacing = sendMessage ToggleSpacing

instance LayoutModifier Spacing a where
  -- With edges, shrink the screen so outer gaps equal inner ones.
  modifyLayout sp ws@(Workspace _ _ ms) r
    | active sp ms, spacingEdge sp = runLayout ws (inset (screenInsets sp) r)
    | otherwise = runLayout ws r

  pureModifier sp _ ms wrs
    | active sp ms = ([ (w, inset (windowInsets sp) wr) | (w, wr) <- wrs ], Nothing)
    | otherwise = (wrs, Nothing)

  pureMess sp m = case fromMessage m of
    Just (ModifySpacing f) -> Just sp { spacingSize = f (spacingSize sp) }
    Just ToggleSpacing -> Just sp { spacingEnabled = not (spacingEnabled sp) }
    Nothing -> Nothing

  modifierDescription sp = if spacingSize sp > 0 && spacingEnabled sp then "Spacing" else ""

active :: Spacing a -> Maybe (Stack a) -> Bool
active sp ms = spacingEnabled sp && spacingSize sp > 0
  && not (spacingSmart sp && maybe True single ms)
  where single st = length (integrate st) <= 1

-- | (left, top, right, bottom) taken from each window.
--
-- Exact mode splits the gap between the two neighbours: the left/top one
-- loses @n - n/2@ on its right/bottom side and the right/bottom one @n/2@
-- on its left/top side, which adds up to @n@ even when @n@ is odd.
windowInsets :: Spacing a -> (Int, Int, Int, Int)
windowInsets sp
  | spacingExact sp = (lo, lo, hi, hi)
  | otherwise = (n, n, n, n)
  where
    n = spacingSize sp
    lo = n `div` 2
    hi = n - lo

-- | (left, top, right, bottom) taken from the screen, so that together with
-- the window insets every edge also ends up @n@ (exact) or @2n@ away.
screenInsets :: Spacing a -> (Int, Int, Int, Int)
screenInsets sp
  | spacingExact sp = (hi, hi, lo, lo)
  | otherwise = (n, n, n, n)
  where
    n = spacingSize sp
    lo = n `div` 2
    hi = n - lo

inset :: (Int, Int, Int, Int) -> Rectangle -> Rectangle
inset (l, t, r, b) (Rectangle x y w h) =
  Rectangle (x + l) (y + t) (max 1 (w - l - r)) (max 1 (h - t - b))
