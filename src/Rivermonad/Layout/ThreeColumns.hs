{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Three columns, as in @XMonad.Layout.ThreeColumns@: the master in the
-- left ('ThreeCol') or middle ('ThreeColMid') column, the rest split between
-- the other two.
--
-- > layoutHook = Layout (ThreeColMid 1 (3/100) (1/2) ||| Tall 1 (3/100) (1/2))
module Rivermonad.Layout.ThreeColumns
  ( ThreeCol (..)
  ) where

import Control.Monad (msum)

import Rivermonad.Core
import Rivermonad.Layout (IncMasterN (..), Resize (..), splitHorizontallyBy, splitVertically)
import Rivermonad.StackSet (integrate)

data ThreeCol a
  = ThreeCol    { threeColNMaster :: !Int, threeColDelta :: !Rational, threeColFrac :: !Rational }
  | ThreeColMid { threeColNMaster :: !Int, threeColDelta :: !Rational, threeColFrac :: !Rational }
  deriving (Show, Read)

instance LayoutClass ThreeCol a where
  pureLayout l r s = zip ws (tile3 (isMid l) (threeColFrac l) r (threeColNMaster l) (length ws))
    where
      ws = integrate s
      isMid ThreeColMid {} = True
      isMid _ = False
  pureMessage l m = msum [fmap resize (fromMessage m), fmap incmaster (fromMessage m)]
    where
      resize Shrink = l { threeColFrac = max (-0.5) (threeColFrac l - threeColDelta l) }
      resize Expand = l { threeColFrac = min 1 (threeColFrac l + threeColDelta l) }
      incmaster (IncMasterN k) = l { threeColNMaster = max 0 (threeColNMaster l + k) }
  description ThreeCol {} = "ThreeCol"
  description ThreeColMid {} = "ThreeColMid"

tile3 :: Bool -> Rational -> Rectangle -> Int -> Int -> [Rectangle]
tile3 middle f r nmaster n
  | n <= nmaster || nmaster == 0 = splitVertically n r
  | n <= nmaster + 1 = splitVertically nmaster s1 ++ splitVertically (n - nmaster) s2
  | otherwise = splitVertically nmaster r1 ++ splitVertically nslave1 r2 ++ splitVertically nslave2 r3
  where
    (r1, r2, r3) = split3HorizontallyBy middle (if f < 0 then 1 + 2 * f else f) r
    (s1, s2) = splitHorizontallyBy (if f < 0 then 1 + f else f) r
    nslave = n - nmaster
    nslave1 = (nslave + 1) `div` 2
    nslave2 = nslave - nslave1

split3HorizontallyBy :: Bool -> Rational -> Rectangle -> (Rectangle, Rectangle, Rectangle)
split3HorizontallyBy middle f (Rectangle sx sy sw sh)
  | middle = ( Rectangle (sx + r3w) sy r1w sh
             , Rectangle (sx + r3w + r1w) sy r2w sh
             , Rectangle sx sy r3w sh )
  | otherwise = ( Rectangle sx sy r1w sh
                , Rectangle (sx + r1w) sy r2w sh
                , Rectangle (sx + r1w + r2w) sy r3w sh )
  where
    r1w = ceiling (fromIntegral sw * f)
    r2w = (sw - r1w + 1) `div` 2
    r3w = sw - r1w - r2w
