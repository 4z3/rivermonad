-- | Extra manage hook helpers, as in @XMonad.Hooks.ManageHelpers@.
--
-- > manageHook = composeOne
-- >   [ isDialog                 -?> doCenterFloat
-- >   , appId =? "mpv"           -?> doFullFloat
-- >   , appId ^? "org.gnome."    -?> doFloat
-- >   , transience
-- >   ]
module Rivermonad.Hooks.ManageHelpers
  ( MaybeManageHook
  , composeOne
  , (-?>), (-->>), (/=?), (^?), (~?), ($?)
  , doRectFloat
  , doFullFloat
  , doCenteredRect
  , currentWs
  , transience
  , transience'
  , isFullscreen
  ) where

import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Data.Monoid (Endo (..))
import qualified Data.Set as S

import Rivermonad.Core
import Rivermonad.ManageHook
import qualified Rivermonad.StackSet as W

type MaybeManageHook = Query (Maybe (Endo WindowSet))

infixr 0 -?>, -->>
infix 4 /=?, ^?, ~?, $?

-- | Run the first hook whose condition matches.
composeOne :: [MaybeManageHook] -> ManageHook
composeOne = foldr try (pure mempty)
  where try q z = q >>= maybe z pure

(-?>) :: Query Bool -> ManageHook -> MaybeManageHook
p -?> f = p >>= \b -> if b then Just <$> f else pure Nothing

-- | Like '-->' but the action also gets the matched value.
(-->>) :: Query (Maybe a) -> (a -> ManageHook) -> ManageHook
q -->> f = q >>= maybe (pure mempty) f

(/=?) :: Eq a => Query a -> a -> Query Bool
q /=? x = (/= x) <$> q

-- | Prefix, infix and suffix matches.
(^?), (~?), ($?) :: Eq a => Query [a] -> [a] -> Query Bool
q ^? x = (x `isPrefixOf`) <$> q
q ~? x = (x `isInfixOf`) <$> q
q $? x = (x `isSuffixOf`) <$> q

-- | Float at a given position and size relative to the screen.
doRectFloat :: W.RationalRect -> ManageHook
doRectFloat r = ask >>= \w -> doF (W.float w r)

doFullFloat :: ManageHook
doFullFloat = doRectFloat (W.RationalRect 0 0 1 1)

-- | Float centred, taking the given fractions of the screen.
doCenteredRect :: (Rational, Rational) -> ManageHook
doCenteredRect (w, h) = doRectFloat (W.RationalRect ((1 - w) / 2) ((1 - h) / 2) w h)

-- | The workspace the window is being placed on.
currentWs :: Query WorkspaceId
currentWs = liftX (gets (W.currentTag . windowset))

-- | Put dialogs and other child windows on their parent's workspace.
transience :: MaybeManageHook
transience = do
  mp <- parentWindow
  ws <- liftX (gets windowset)
  case mp >>= \p -> W.findTag p ws of
    Just t -> Just <$> doShift t
    Nothing -> pure Nothing

transience' :: ManageHook
transience' = maybe mempty id <$> transience

-- | The window asked to be fullscreen.
isFullscreen :: Query Bool
isFullscreen = do
  w <- ask
  liftX (gets (S.member w . fullscreen))
