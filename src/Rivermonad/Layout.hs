{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
-- | The standard layouts and messages, as in @XMonad.Layout@.
module Rivermonad.Layout
  ( -- * Re-exported from the core
    Rectangle (..)
  , LayoutClass (..)
  , Layout (..)
  , Message
  , SomeMessage (..)
  , fromMessage
    -- * Messages
  , Resize (..)
  , IncMasterN (..)
  , ChangeLayout (..)
  , JumpToLayout (..)
  , sendLayoutMessage
    -- * Layouts
  , Tall (..)
  , Full (..)
  , Mirror (..)
  , Choose
  , (|||)
  , tile
  , splitVertically
  , splitHorizontallyBy
  , splitHorizontally
  , mirrorRect
  ) where

import Control.Monad (msum)
import Rivermonad.Core
import Rivermonad.StackSet (Workspace (..), integrate)

-- | Change the master area size.
data Resize = Shrink | Expand deriving (Show, Eq)
instance Message Resize

-- | Change the number of windows in the master area.
newtype IncMasterN = IncMasterN Int deriving (Show, Eq)
instance Message IncMasterN

-- | Cycle through the layouts combined with '|||'.
data ChangeLayout = NextLayout | FirstLayout deriving (Show, Eq)
instance Message ChangeLayout

-- | Select a layout by its 'description'.
newtype JumpToLayout = JumpToLayout String deriving (Show, Eq)
instance Message JumpToLayout

-- | Deliver a message to a top-level layout; a 'NextLayout' that falls off
-- the end of the '|||' chain wraps around to the first layout.
sendLayoutMessage :: Layout a -> SomeMessage -> X (Maybe (Layout a))
sendLayoutMessage l m = do
  r <- handleMessage l m
  case r of
    Nothing | Just NextLayout <- fromMessage m -> handleMessage l (SomeMessage FirstLayout)
    _ -> pure r

------------------------------------------------------------------------------
-- Full

data Full a = Full deriving (Show, Read)

instance LayoutClass Full a

------------------------------------------------------------------------------
-- Tall

-- | The classic master/stack layout.
data Tall a = Tall
  { tallNMaster        :: !Int       -- ^ windows in the master pane
  , tallRatioIncrement :: !Rational  -- ^ step used by Shrink/Expand
  , tallRatio          :: !Rational  -- ^ fraction of the width for the master pane
  } deriving (Show, Read)

instance LayoutClass Tall a where
  pureLayout (Tall n _ f) r s = zip ws (tile f r n (length ws))
    where ws = integrate s
  pureMessage (Tall n d f) m = msum [fmap resize (fromMessage m), fmap incmaster (fromMessage m)]
    where
      resize Shrink = Tall n d (max 0 (f - d))
      resize Expand = Tall n d (min 1 (f + d))
      incmaster (IncMasterN k) = Tall (max 0 (n + k)) d f
  description _ = "Tall"

tile :: Rational -> Rectangle -> Int -> Int -> [Rectangle]
tile f r nmaster n
  | n <= nmaster || nmaster == 0 = splitVertically n r
  | otherwise = splitVertically nmaster r1 ++ splitVertically (n - nmaster) r2
  where (r1, r2) = splitHorizontallyBy f r

splitVertically :: Int -> Rectangle -> [Rectangle]
splitVertically n r | n < 2 = [r]
splitVertically n (Rectangle sx sy sw sh) =
  Rectangle sx sy sw smallh : splitVertically (n - 1) (Rectangle sx (sy + smallh) sw (sh - smallh))
  where smallh = sh `div` n

splitHorizontally :: Int -> Rectangle -> [Rectangle]
splitHorizontally n = map mirrorRect . splitVertically n . mirrorRect

splitHorizontallyBy :: Rational -> Rectangle -> (Rectangle, Rectangle)
splitHorizontallyBy f (Rectangle sx sy sw sh) =
  (Rectangle sx sy leftw sh, Rectangle (sx + leftw) sy (sw - leftw) sh)
  where leftw = floor (fromIntegral sw * f)

------------------------------------------------------------------------------
-- Mirror

-- | Rotate a layout by 90 degrees.
newtype Mirror l a = Mirror (l a)

deriving instance Show (l a) => Show (Mirror l a)
deriving instance Read (l a) => Read (Mirror l a)

instance LayoutClass l a => LayoutClass (Mirror l) a where
  runLayout (Workspace i (Mirror l) ms) r = do
    (wrs, ml') <- runLayout (Workspace i l ms) (mirrorRect r)
    pure ([ (w, mirrorRect wr) | (w, wr) <- wrs ], Mirror <$> ml')
  handleMessage (Mirror l) m = fmap Mirror <$> handleMessage l m
  description (Mirror l) = "Mirror " ++ description l

mirrorRect :: Rectangle -> Rectangle
mirrorRect (Rectangle x y w h) = Rectangle y x h w

------------------------------------------------------------------------------
-- Choose (|||)

data LR = L | R deriving (Show, Read, Eq)

-- | Two layouts, one of them active.
data Choose l r a = Choose LR (l a) (r a)

deriving instance (Show (l a), Show (r a)) => Show (Choose l r a)
deriving instance (Read (l a), Read (r a)) => Read (Choose l r a)

infixr 5 |||
(|||) :: l a -> r a -> Choose l r a
(|||) = Choose L

instance (LayoutClass l a, LayoutClass r a) => LayoutClass (Choose l r) a where
  runLayout (Workspace i (Choose L l r) ms) rect = do
    (wrs, ml') <- runLayout (Workspace i l ms) rect
    pure (wrs, (\l' -> Choose L l' r) <$> ml')
  runLayout (Workspace i (Choose R l r) ms) rect = do
    (wrs, mr') <- runLayout (Workspace i r ms) rect
    pure (wrs, Choose R l <$> mr')

  description (Choose L l _) = description l
  description (Choose R _ r) = description r

  handleMessage (Choose d l r) m
    | Just NextLayout <- fromMessage m = case d of
        -- Let a nested Choose advance first; when it runs out, move right.
        L -> handleMessage l m >>= \case
          Just l' -> pure (Just (Choose L l' r))
          Nothing -> Just . Choose R l <$> reset r
        R -> fmap (Choose R l) <$> handleMessage r m
    | Just FirstLayout <- fromMessage m = do
        l' <- reset l
        pure (Just (Choose L l' r))
    | Just (JumpToLayout n) <- fromMessage m = do
        jl <- jump n l
        case jl of
          Just l' -> pure (Just (Choose L l' r))
          Nothing -> fmap (Choose R l) <$> jump n r
    | otherwise = case d of
        L -> fmap (\l' -> Choose L l' r) <$> handleMessage l m
        R -> fmap (Choose R l) <$> handleMessage r m
    where
      reset x = maybe x id <$> handleMessage x (SomeMessage FirstLayout)
      jump n x
        | description x == n = pure (Just x)
        | otherwise = handleMessage x (SomeMessage (JumpToLayout n))
