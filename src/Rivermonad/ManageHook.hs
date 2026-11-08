-- | Combinators for manage hooks, as in @XMonad.ManageHook@.
--
-- > myManageHook = composeAll
-- >   [ appId =? "pavucontrol" --> doCenterFloat
-- >   , appId =? "firefox"     --> doShift "2"
-- >   , isDialog               --> doFloat ]
module Rivermonad.ManageHook
  ( appId, className, title, identifier, parentWindow
  , (=?), (<&&>), (<||>), (-->)
  , composeAll, idHook
  , doF, doShift, doFloat, doCenterFloat, doSink, doFullscreen
  , isDialog, isFixedSize
  ) where

import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Data.Monoid (Endo (..))
import qualified Data.Set as S

import Rivermonad.Core
import qualified Rivermonad.StackSet as W

infixr 3 <&&>, <||>
infix 0 -->

info :: (WindowInfo -> a) -> a -> Query a
info f d = do
  w <- ask
  liftX (gets (maybe d f . M.lookup w . windowInfo))

-- | The Wayland app_id (the closest thing to the X11 WM_CLASS).
appId :: Query String
appId = info wiAppId ""

-- | Alias of 'appId' so xmonad configs keep working.
className :: Query String
className = appId

title :: Query String
title = info wiTitle ""

-- | river's unique, stable window identifier.
identifier :: Query String
identifier = info wiIdentifier ""

parentWindow :: Query (Maybe Window)
parentWindow = info wiParent Nothing

-- | Dialogs, file pickers and other windows with a parent.
isDialog :: Query Bool
isDialog = isJust <$> parentWindow

-- | Windows whose minimum and maximum sizes are equal.
isFixedSize :: Query Bool
isFixedSize = info (\i -> let (mw, mh) = wiMinSize i; (xw, xh) = wiMaxSize i
                         in xw > 0 && xh > 0 && mw == xw && mh == xh) False

(=?) :: Eq a => Query a -> a -> Query Bool
q =? x = (== x) <$> q

(<&&>), (<||>) :: Monad m => m Bool -> m Bool -> m Bool
a <&&> b = a >>= \x -> if x then b else pure False
a <||> b = a >>= \x -> if x then pure True else b

(-->) :: Monoid m => Query Bool -> Query m -> Query m
p --> f = p >>= \b -> if b then f else pure mempty

composeAll :: [ManageHook] -> ManageHook
composeAll = mconcat

idHook :: ManageHook
idHook = mempty

doF :: (WindowSet -> WindowSet) -> ManageHook
doF = pure . Endo

doShift :: WorkspaceId -> ManageHook
doShift i = ask >>= doF . W.shiftWin i

-- | Float the window, centred on its screen at the size it picks itself.
doFloat :: ManageHook
doFloat = do
  w <- ask
  liftX $ modify $ \s -> s { needsCenter = S.insert w (needsCenter s) }
  doF (W.float w (W.RationalRect (1 / 4) (1 / 4) (1 / 2) (1 / 2)))

-- | Same as 'doFloat': new Wayland windows have no position of their own.
doCenterFloat :: ManageHook
doCenterFloat = doFloat

doSink :: ManageHook
doSink = ask >>= doF . W.sink

doFullscreen :: ManageHook
doFullscreen = do
  w <- ask
  liftX $ modify $ \s -> s { fullscreen = S.insert w (fullscreen s) }
  pure mempty
