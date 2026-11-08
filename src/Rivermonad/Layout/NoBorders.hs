{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Remove window borders, as in @XMonad.Layout.NoBorders@.
--
-- > layoutHook = Layout (smartBorders (tiled ||| Mirror tiled ||| noBorders Full))
module Rivermonad.Layout.NoBorders
  ( noBorders
  , smartBorders
  , WithBorder
  , SmartBorder
  ) where

import qualified Data.Map.Strict as M

import Rivermonad.Core
import Rivermonad.Layout.LayoutModifier
import qualified Rivermonad.StackSet as W

-- | Remove borders from every tiled window of the layout.
noBorders :: l a -> ModifiedLayout WithBorder l a
noBorders = ModifiedLayout (WithBorder 0)

newtype WithBorder a = WithBorder Int deriving (Show, Read)

instance LayoutModifier WithBorder Window where
  redoLayout (WithBorder n) _ _ wrs = do
    setBorders n (map fst wrs)
    pure (wrs, Nothing)

-- | Remove the border when it carries no information: a single tiled window
-- with no floating windows on the workspace.
smartBorders :: l a -> ModifiedLayout SmartBorder l a
smartBorders = ModifiedLayout SmartBorder

data SmartBorder a = SmartBorder deriving (Show, Read)

instance LayoutModifier SmartBorder Window where
  redoLayout _ _ ms wrs = do
    flt <- gets (W.floating . windowset)
    let floatsHere = any (`M.member` flt) (W.integrate' ms)
    case wrs of
      [(w, _)] | not floatsHere -> setBorders 0 [w]
      _ -> pure ()
    pure (wrs, Nothing)

setBorders :: Int -> [Window] -> X ()
setBorders n ws = modify $ \s ->
  s { borderWidths = foldr (\w -> M.insert w n) (borderWidths s) ws }
