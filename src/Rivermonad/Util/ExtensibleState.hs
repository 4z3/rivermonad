{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- | Typed, per-module state stored in the window manager, as in
-- @XMonad.Util.ExtensibleState@. Import it qualified:
--
-- > import qualified Rivermonad.Util.ExtensibleState as XS
-- >
-- > newtype Counter = Counter Int deriving (Read, Show)
-- > instance ExtensionClass Counter where
-- >   initialValue = Counter 0
-- >   extensionType = PersistentExtension   -- survive restarts
-- >
-- > bump :: X ()
-- > bump = XS.modify (\(Counter n) -> Counter (n + 1))
module Rivermonad.Util.ExtensibleState
  ( put
  , get
  , gets
  , modify
  , modifyM
  , remove
  ) where

import qualified Data.Map.Strict as M
import Data.Typeable (Proxy (..), cast, typeRep)

import Rivermonad.Core hiding (get, gets, modify, put)
import qualified Rivermonad.Core as C

key :: forall a. ExtensionClass a => Proxy a -> String
key _ = show (typeRep (Proxy @a))

put :: forall a. ExtensionClass a => a -> X ()
put v = C.modify $ \s ->
  s { extensibleState = M.insert (key (Proxy @a)) (Right (extensionType v)) (extensibleState s) }

get :: forall a. ExtensionClass a => X a
get = do
  v <- C.gets (M.lookup (key (Proxy @a)) . extensibleState)
  case v of
    Just (Right (StateExtension x)) -> pure (maybe initialValue id (cast x))
    Just (Right (PersistentExtension x)) -> pure (maybe initialValue id (cast x))
    -- Saved across a restart as text: parse it on first use.
    Just (Left str) -> case extensionType (initialValue :: a) of
      PersistentExtension (_ :: b) -> do
        let parsed = case reads str :: [(b, String)] of
              [(x, "")] -> cast x
              _ -> Nothing
            val = maybe initialValue id parsed
        put val
        pure val
      StateExtension _ -> pure initialValue
    Nothing -> pure initialValue

gets :: ExtensionClass a => (a -> b) -> X b
gets f = f <$> get

modify :: ExtensionClass a => (a -> a) -> X ()
modify f = get >>= put . f

modifyM :: ExtensionClass a => (a -> X a) -> X ()
modifyM f = get >>= f >>= put

remove :: forall a. ExtensionClass a => Proxy a -> X ()
remove p = C.modify $ \s -> s { extensibleState = M.delete (key p) (extensibleState s) }
