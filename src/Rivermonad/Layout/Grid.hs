{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Windows in a grid, as in @XMonad.Layout.Grid@.
--
-- > layoutHook = Layout (Grid ||| Tall 1 (3/100) (1/2))
module Rivermonad.Layout.Grid
  ( Grid (..)
  , arrange
  , defaultRatio
  ) where

import Rivermonad.Core
import Rivermonad.StackSet (integrate)

-- | 'Grid' aims for square cells; @GridRatio r@ for cells of aspect ratio r.
data Grid a = Grid | GridRatio Double deriving (Show, Read)

defaultRatio :: Double
defaultRatio = 1

instance LayoutClass Grid a where
  pureLayout Grid r = arrange defaultRatio r . integrate
  pureLayout (GridRatio d) r = arrange d r . integrate
  description _ = "Grid"

-- | Fill columns left to right; columns on the right get the extra windows.
arrange :: Double -> Rectangle -> [a] -> [(a, Rectangle)]
arrange _ _ [] = []
arrange ratio (Rectangle rx ry rw rh) ws = zip ws rects
  where
    n = length ws
    ncols = max 1 (min n (round (sqrt (fromIntegral n * fromIntegral rw / (fromIntegral rh * ratio)) :: Double)))
    base = n `div` ncols                 -- windows per column
    extra = n - ncols * base             -- the last 'extra' columns get one more
    perCol = replicate (ncols - extra) base ++ replicate extra (base + 1)
    cols = chop ncols rw
    rects = concat [ [ Rectangle (rx + x) (ry + y) w h | (y, h) <- chop k rh ]
                   | ((x, w), k) <- zip cols perCol ]

-- | Split a length into n (offset, size) pieces; the first absorbs rounding.
chop :: Int -> Int -> [(Int, Int)]
chop n total = zip offsets sizes
  where
    k = total `div` n
    sizes = (total - k * (n - 1)) : replicate (n - 1) k
    offsets = scanl (+) 0 sizes
