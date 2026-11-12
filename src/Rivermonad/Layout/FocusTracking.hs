{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Keep showing the tiled window that had the focus when the focus moves
-- to a floating window, as in @XMonad.Layout.FocusTracking@.
--
-- A layout only sees the tiled windows. When a floating window is focused,
-- the layout is told that some other tiled window has the focus, so 'Full'
-- would suddenly show a different window under the float. With
-- 'focusTracking' it keeps showing the one that was focused before.
--
-- > layoutHook = Layout (Tall 1 (3/100) (1/2) ||| focusTracking Full)
module Rivermonad.Layout.FocusTracking
  ( focusTracking
  , FocusTracking (..)
  , focusOn
  ) where

import Control.Applicative ((<|>))

import Rivermonad.Core
import Rivermonad.Layout.LayoutModifier
import Rivermonad.StackSet (Stack (..), Workspace (..))
import qualified Rivermonad.StackSet as W

-- | The tiled window that last really had the focus.
newtype FocusTracking a = FocusTracking (Maybe a)

-- The remembered window is not written to the restart state: window ids
-- do not survive a restart.
instance Show (FocusTracking a) where
  showsPrec _ _ = showString "FocusTracking"

instance Read (FocusTracking a) where
  readsPrec _ s = [ (FocusTracking Nothing, rest) | ("FocusTracking", rest) <- lex s ]

focusTracking :: l a -> ModifiedLayout FocusTracking l a
focusTracking = ModifiedLayout (FocusTracking Nothing)

instance LayoutModifier FocusTracking Window where
  modifyLayout (FocusTracking old) (Workspace t l st) r = do
    real <- gets (W.peek . windowset)
    let given = W.focus <$> st
        passed
          | real == given = st
          | otherwise = (old >>= \o -> st >>= focusOn o) <|> st
    runLayout (Workspace t l passed) r

  redoLayout (FocusTracking old) _ st wrs = do
    real <- gets (W.peek . windowset)
    let given = W.focus <$> st
    pure (wrs, if real == given && given /= old then Just (FocusTracking given) else Nothing)

-- | The same stack with another member focused; 'Nothing' if it is not in
-- the stack.
focusOn :: Eq a => a -> Stack a -> Maybe (Stack a)
focusOn x s = case break (== x) (W.integrate s) of
  (before, _ : after) -> Just (Stack x (reverse before) after)
  _ -> Nothing
