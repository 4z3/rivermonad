{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Toggle between two layouts, as in @XMonad.Layout.ToggleLayouts@.
--
-- > layoutHook = Layout (toggleLayouts (noBorders Full) (tiled ||| Mirror tiled))
-- > , ("M-z", sendMessage ToggleLayout)
module Rivermonad.Layout.ToggleLayouts
  ( toggleLayouts
  , ToggleLayouts
  , ToggleLayout (..)
  ) where

import Rivermonad.Core
import Rivermonad.StackSet (Workspace (..))

-- | @ToggleLayouts on lt lf@: when @on@, the first layout is active.
data ToggleLayouts lt lf a = ToggleLayouts Bool (lt a) (lf a)

deriving instance (Show (lt a), Show (lf a)) => Show (ToggleLayouts lt lf a)
deriving instance (Read (lt a), Read (lf a)) => Read (ToggleLayouts lt lf a)

data ToggleLayout = ToggleLayout | Toggle String deriving (Show, Eq)
instance Message ToggleLayout

-- | Start with the second layout; 'ToggleLayout' switches to the first.
toggleLayouts :: lt a -> lf a -> ToggleLayouts lt lf a
toggleLayouts = ToggleLayouts False

instance (LayoutClass lt a, LayoutClass lf a) => LayoutClass (ToggleLayouts lt lf) a where
  runLayout (Workspace i (ToggleLayouts True lt lf) ms) r = do
    (wrs, mlt) <- runLayout (Workspace i lt ms) r
    pure (wrs, (\lt' -> ToggleLayouts True lt' lf) <$> mlt)
  runLayout (Workspace i (ToggleLayouts False lt lf) ms) r = do
    (wrs, mlf) <- runLayout (Workspace i lf ms) r
    pure (wrs, ToggleLayouts False lt <$> mlf)

  description (ToggleLayouts True lt _) = description lt
  description (ToggleLayouts False _ lf) = description lf

  handleMessage (ToggleLayouts on lt lf) m
    | Just ToggleLayout <- fromMessage m = pure (Just (ToggleLayouts (not on) lt lf))
    | Just (Toggle d) <- fromMessage m
    , d == description lt || d == description lf = pure (Just (ToggleLayouts (not on) lt lf))
    | on = fmap (\lt' -> ToggleLayouts True lt' lf) <$> handleMessage lt m
    | otherwise = fmap (ToggleLayouts False lt) <$> handleMessage lf m
