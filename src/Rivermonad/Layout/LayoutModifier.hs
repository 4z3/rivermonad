{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Wrap a layout to alter what it does, without writing a full layout.
-- A port of @XMonad.Layout.LayoutModifier@ (xmonad-contrib, BSD-3).
--
-- A modifier can change the rectangle given to the inner layout
-- ('modifyLayout'), post-process its result ('redoLayout' / 'pureModifier')
-- and react to messages ('handleMess' / 'pureMess').
module Rivermonad.Layout.LayoutModifier
  ( LayoutModifier (..)
  , ModifiedLayout (..)
  ) where

import Rivermonad.Core
import Rivermonad.StackSet (Stack, Workspace (..))

class (Show (m a), Read (m a)) => LayoutModifier m a where
  -- | Run the inner layout; the default just runs it unchanged.
  modifyLayout :: LayoutClass l a => m a -> Workspace WorkspaceId (l a) a -> Rectangle
               -> X ([(a, Rectangle)], Maybe (l a))
  modifyLayout _ = runLayout

  -- | Post-process the window rectangles produced by the inner layout.
  redoLayout :: m a -> Rectangle -> Maybe (Stack a) -> [(a, Rectangle)]
             -> X ([(a, Rectangle)], Maybe (m a))
  redoLayout m r ms wrs = pure (pureModifier m r ms wrs)

  pureModifier :: m a -> Rectangle -> Maybe (Stack a) -> [(a, Rectangle)]
               -> ([(a, Rectangle)], Maybe (m a))
  pureModifier _ _ _ wrs = (wrs, Nothing)

  handleMess :: m a -> SomeMessage -> X (Maybe (m a))
  handleMess m = pure . pureMess m

  pureMess :: m a -> SomeMessage -> Maybe (m a)
  pureMess _ _ = Nothing

  -- | Prefix added to the inner layout's description ("" = unchanged).
  modifierDescription :: m a -> String
  modifierDescription _ = ""

  modifyDescription :: LayoutClass l a => m a -> l a -> String
  modifyDescription m l = case modifierDescription m of
    "" -> description l
    d  -> d ++ " " ++ description l

data ModifiedLayout m l a = ModifiedLayout (m a) (l a)

deriving instance (Show (m a), Show (l a)) => Show (ModifiedLayout m l a)
deriving instance (Read (m a), Read (l a)) => Read (ModifiedLayout m l a)

instance (LayoutModifier m a, LayoutClass l a) => LayoutClass (ModifiedLayout m l) a where
  runLayout (Workspace i (ModifiedLayout m l) ms) r = do
    (wrs, ml') <- modifyLayout m (Workspace i l ms) r
    (wrs', mm') <- redoLayout m r ms wrs
    let result = case mm' of
          Just m' -> Just (ModifiedLayout m' (maybe l id ml'))
          Nothing -> ModifiedLayout m <$> ml'
    pure (wrs', result)

  -- Both the modifier and the inner layout see every message.
  handleMessage (ModifiedLayout m l) msg = do
    mm' <- handleMess m msg
    ml' <- handleMessage l msg
    pure $ case (mm', ml') of
      (Nothing, Nothing) -> Nothing
      _ -> Just (ModifiedLayout (maybe m id mm') (maybe l id ml'))

  description (ModifiedLayout m l) = modifyDescription m l
