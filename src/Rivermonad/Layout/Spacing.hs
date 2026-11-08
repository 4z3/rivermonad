{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Gaps between windows (and optionally at the screen edges), a simplified
-- @XMonad.Layout.Spacing@.
--
-- > layoutHook = Layout (spacingWithEdge 6 (Tall 1 (3/100) (1/2)))
-- > , ("M-=", incSpacing 2), ("M--", incSpacing (-2)), ("M-g", toggleSpacing)
module Rivermonad.Layout.Spacing
  ( spacing
  , spacingWithEdge
  , smartSpacing
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
  { spacingSize    :: !Int    -- ^ gap around each window (2x between windows)
  , spacingEdge    :: !Bool   -- ^ also leave a gap at the screen edges
  , spacingSmart   :: !Bool   -- ^ no gaps when there is a single window
  , spacingEnabled :: !Bool
  } deriving (Show, Read)

-- | Gaps between windows only.
spacing :: Int -> l a -> ModifiedLayout Spacing l a
spacing n = ModifiedLayout (Spacing n False False True)

-- | Gaps between windows and at the screen edges (uniform everywhere).
spacingWithEdge :: Int -> l a -> ModifiedLayout Spacing l a
spacingWithEdge n = ModifiedLayout (Spacing n True False True)

-- | Like 'spacingWithEdge' but without gaps for a lone window.
smartSpacing :: Int -> l a -> ModifiedLayout Spacing l a
smartSpacing n = ModifiedLayout (Spacing n True True True)

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
    | active sp ms, spacingEdge sp = runLayout ws (shrink (spacingSize sp) r)
    | otherwise = runLayout ws r

  pureModifier sp _ ms wrs
    | active sp ms = ([ (w, shrink (spacingSize sp) wr) | (w, wr) <- wrs ], Nothing)
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

shrink :: Int -> Rectangle -> Rectangle
shrink n (Rectangle x y w h) = Rectangle (x + n) (y + n) (max 1 (w - 2 * n)) (max 1 (h - 2 * n))
