{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Different layouts on different workspaces, as in
-- @XMonad.Layout.PerWorkspace@.
--
-- > layoutHook = Layout (onWorkspace "web" Full (Tall 1 (3/100) (1/2) ||| Full))
module Rivermonad.Layout.PerWorkspace
  ( onWorkspace
  , onWorkspaces
  , PerWorkspace
  ) where

import Rivermonad.Core
import Rivermonad.StackSet (Workspace (..))

data PerWorkspace l1 l2 a = PerWorkspace [WorkspaceId] Bool (l1 a) (l2 a)

deriving instance (Show (l1 a), Show (l2 a)) => Show (PerWorkspace l1 l2 a)
deriving instance (Read (l1 a), Read (l2 a)) => Read (PerWorkspace l1 l2 a)

onWorkspace :: WorkspaceId -> l1 a -> l2 a -> PerWorkspace l1 l2 a
onWorkspace t = onWorkspaces [t]

onWorkspaces :: [WorkspaceId] -> l1 a -> l2 a -> PerWorkspace l1 l2 a
onWorkspaces ts = PerWorkspace ts False

instance (LayoutClass l1 a, LayoutClass l2 a) => LayoutClass (PerWorkspace l1 l2) a where
  runLayout (Workspace i (PerWorkspace ts _ l1 l2) ms) r
    | i `elem` ts = do
        (wrs, ml1) <- runLayout (Workspace i l1 ms) r
        pure (wrs, Just (PerWorkspace ts True (maybe l1 id ml1) l2))
    | otherwise = do
        (wrs, ml2) <- runLayout (Workspace i l2 ms) r
        pure (wrs, Just (PerWorkspace ts False l1 (maybe l2 id ml2)))

  handleMessage (PerWorkspace ts True l1 l2) m =
    fmap (\l1' -> PerWorkspace ts True l1' l2) <$> handleMessage l1 m
  handleMessage (PerWorkspace ts False l1 l2) m =
    fmap (PerWorkspace ts False l1) <$> handleMessage l2 m

  description (PerWorkspace _ True l1 _) = description l1
  description (PerWorkspace _ False _ l2) = description l2
