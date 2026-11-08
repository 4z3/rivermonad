{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Layouts and layout messages, modelled after @XMonad.Layout@ /
-- @XMonad.Core.LayoutClass@ but pure: a layout maps a rectangle and a stack
-- of windows to window rectangles.
module Rivermonad.Layout
  ( Rectangle (..)
  , LayoutClass (..)
  , Layout (..)
  , runLayout
  , handleMessage
    -- * Messages
  , Message
  , SomeMessage (..)
  , fromMessage
  , Resize (..)
  , IncMasterN (..)
  , ChangeLayout (..)
  , JumpToLayout (..)
    -- * Layouts
  , Tall (..)
  , Full (..)
  , Mirror (..)
  , Choose
  , (|||)
  , tile
  , splitVertically
  , splitHorizontallyBy
  , mirrorRect
  ) where

import Control.Monad (msum)
import Data.Typeable (Typeable, cast)
import Rivermonad.StackSet (Stack (..), integrate)

data Rectangle = Rectangle
  { rect_x      :: !Int
  , rect_y      :: !Int
  , rect_width  :: !Int
  , rect_height :: !Int
  } deriving (Show, Read, Eq, Ord)

------------------------------------------------------------------------------
-- Messages

class Typeable a => Message a

data SomeMessage = forall a. Message a => SomeMessage a

fromMessage :: Message m => SomeMessage -> Maybe m
fromMessage (SomeMessage m) = cast m

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

------------------------------------------------------------------------------
-- Layout class

class Show (l a) => LayoutClass l a where
  -- | Place the windows of a non-empty stack inside the given rectangle.
  pureLayout :: l a -> Rectangle -> Stack a -> [(a, Rectangle)]
  pureLayout _ r s = [(focus s, r)]

  -- | React to a message. 'Nothing' means the layout ignored it.
  pureMessage :: l a -> SomeMessage -> Maybe (l a)
  pureMessage _ _ = Nothing

  -- | Human readable name, used by 'JumpToLayout' and status bars.
  description :: l a -> String
  description = takeWhile (/= ' ') . show

-- | A layout of unknown type.
data Layout a = forall l. LayoutClass l a => Layout (l a)

instance Show (Layout a) where
  show (Layout l) = show l

instance LayoutClass Layout a where
  pureLayout (Layout l) = pureLayout l
  pureMessage (Layout l) m = Layout <$> pureMessage l m
  description (Layout l) = description l

runLayout :: Layout a -> Rectangle -> Maybe (Stack a) -> [(a, Rectangle)]
runLayout _ _ Nothing = []
runLayout l r (Just s) = pureLayout l r s

-- | Deliver a message; a top-level 'NextLayout' that falls off the end of
-- the '|||' chain wraps around to the first layout.
handleMessage :: Layout a -> SomeMessage -> Maybe (Layout a)
handleMessage l m = case pureMessage l m of
  Nothing | Just NextLayout <- fromMessage m -> pureMessage l (SomeMessage FirstLayout)
  r -> r

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

splitHorizontallyBy :: Rational -> Rectangle -> (Rectangle, Rectangle)
splitHorizontallyBy f (Rectangle sx sy sw sh) =
  (Rectangle sx sy leftw sh, Rectangle (sx + leftw) sy (sw - leftw) sh)
  where leftw = floor (fromIntegral sw * f)

------------------------------------------------------------------------------
-- Mirror

-- | Rotate a layout by 90 degrees.
newtype Mirror l a = Mirror (l a)

deriving instance Show (l a) => Show (Mirror l a)

instance LayoutClass l a => LayoutClass (Mirror l) a where
  pureLayout (Mirror l) r s = [ (w, mirrorRect wr) | (w, wr) <- pureLayout l (mirrorRect r) s ]
  pureMessage (Mirror l) m = Mirror <$> pureMessage l m
  description (Mirror l) = "Mirror " ++ description l

mirrorRect :: Rectangle -> Rectangle
mirrorRect (Rectangle x y w h) = Rectangle y x h w

------------------------------------------------------------------------------
-- Choose (|||)

data LR = L | R deriving (Show, Read, Eq)

-- | Two layouts, one of them active.
data Choose l r a = Choose LR (l a) (r a)

deriving instance (Show (l a), Show (r a)) => Show (Choose l r a)

infixr 5 |||
(|||) :: l a -> r a -> Choose l r a
(|||) = Choose L

instance (LayoutClass l a, LayoutClass r a) => LayoutClass (Choose l r) a where
  pureLayout (Choose L l _) = pureLayout l
  pureLayout (Choose R _ r) = pureLayout r

  description (Choose L l _) = description l
  description (Choose R _ r) = description r

  pureMessage (Choose d l r) m
    | Just NextLayout <- fromMessage m = case d of
        -- Let a nested Choose advance first; when it runs out, move right.
        L -> case pureMessage l m of
          Just l' -> Just (Choose L l' r)
          Nothing -> Just (Choose R l (reset r))
        R -> Choose R l <$> pureMessage r m
    | Just FirstLayout <- fromMessage m = Just (Choose L (reset l) r)
    | Just (JumpToLayout n) <- fromMessage m =
        case (jump n l, jump n r) of
          (Just l', _) -> Just (Choose L l' r)
          (_, Just r') -> Just (Choose R l r')
          _            -> Nothing
    | otherwise = case d of
        L -> (\l' -> Choose L l' r) <$> pureMessage l m
        R -> Choose R l <$> pureMessage r m
    where
      reset x = maybe x id (pureMessage x (SomeMessage FirstLayout))
      jump n x
        | description x == n = Just x
        | otherwise          = pureMessage x (SomeMessage (JumpToLayout n))
