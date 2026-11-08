{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | 'Tall' where windows in each column can also be resized vertically, as in
-- @XMonad.Layout.ResizableTile@.
--
-- > layoutHook = Layout (ResizableTall 1 (3/100) (1/2) [])
-- > , ("M-a", sendMessage MirrorShrink), ("M-z", sendMessage MirrorExpand)
module Rivermonad.Layout.ResizableTile
  ( ResizableTall (..)
  , MirrorResize (..)
  ) where

import Rivermonad.Core
import Rivermonad.Layout (IncMasterN (..), Resize (..), splitHorizontallyBy)
import Rivermonad.StackSet (Stack (..), integrate)
import qualified Rivermonad.StackSet as W

data ResizableTall a = ResizableTall
  { rtNMaster :: !Int
  , rtDelta   :: !Rational
  , rtFrac    :: !Rational
  , rtSlaves  :: ![Rational]  -- ^ relative height of each window (1 = normal)
  } deriving (Show, Read)

data MirrorResize = MirrorShrink | MirrorExpand deriving (Show, Eq)
instance Message MirrorResize

instance LayoutClass ResizableTall a where
  pureLayout (ResizableTall nmaster _ frac mfrac) r s =
    zip ws (tile frac (mfrac ++ repeat 1) r nmaster (length ws))
    where ws = integrate s

  handleMessage l m
    | Just Shrink <- fromMessage m = pure (Just l { rtFrac = max 0 (rtFrac l - rtDelta l) })
    | Just Expand <- fromMessage m = pure (Just l { rtFrac = min 1 (rtFrac l + rtDelta l) })
    | Just (IncMasterN d) <- fromMessage m = pure (Just l { rtNMaster = max 0 (rtNMaster l + d) })
    | Just mr <- fromMessage m = do
        -- Needs the focused position, so it runs in X.
        ms <- gets (W.stack . W.workspace . W.current . windowset)
        pure (fmap (mresize mr l) ms)
    | otherwise = pure Nothing

  description _ = "ResizableTall"

-- | Heights are relative shares within a column, so growing the focused
-- window takes space from its neighbours.
mresize :: MirrorResize -> ResizableTall a -> Stack b -> ResizableTall a
mresize mr l s =
  let n = length (up s)
      total = n + length (down s) + 1
      d = case mr of
        MirrorExpand -> rtDelta l * 10
        MirrorShrink -> negate (rtDelta l * 10)
      fr = take total (rtSlaves l ++ repeat 1)
  in l { rtSlaves = [ if j == n then max 0.1 (f + d) else f | (j, f) <- zip [0 :: Int ..] fr ] }

tile :: Rational -> [Rational] -> Rectangle -> Int -> Int -> [Rectangle]
tile f mf r nmaster n
  | n <= nmaster || nmaster == 0 = splitVertically mf n r
  | otherwise = splitVertically mf nmaster r1 ++ splitVertically (drop nmaster mf) (n - nmaster) r2
  where (r1, r2) = splitHorizontallyBy f r

-- | Split vertically, each window taking its relative share.
splitVertically :: [Rational] -> Int -> Rectangle -> [Rectangle]
splitVertically _ n r | n < 2 = [r]
splitVertically fs n (Rectangle sx sy sw sh) = go (take n (fs ++ repeat 1)) sy
  where
    total = sum (take n (fs ++ repeat 1))
    go [] _ = []
    go [_] y = [Rectangle sx y sw (sy + sh - y)]
    go (f : rest) y =
      let h = floor (fromIntegral sh * f / total)
      in Rectangle sx y sw h : go rest (y + h)
