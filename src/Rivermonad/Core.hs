{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Core types: the 'X' monad, layouts, configuration and runtime state.
--
-- As in xmonad, the layout class lives here because layouts run in 'X'.
module Rivermonad.Core
  ( -- * Basic types
    Window, WorkspaceId, ScreenId (..), ScreenDetail (..), WindowSet, WindowSpace
  , Rectangle (..)
  , WindowInfo (..), OutputInfo (..), SeatInfo (..), Drag (..), DragKind (..)
  , Globals (..), Input (..), RestartNow (..)
    -- * Layouts
  , LayoutClass (..), Layout (..), readsLayout
  , Message, SomeMessage (..), fromMessage
    -- * Extensible state
  , ExtensionClass (..), StateExtension (..)
    -- * Configuration
  , XConfig (..), Default (..)
    -- * The X monad
  , X, XConf (..), XState (..), RationalRectS, runX, io, catchX, userCode, userCodeDef
  , withWindowSet, gets, modify, asks, ask, get, put
    -- * Queries / ManageHook
  , Query (..), ManageHook, runQuery, liftX
    -- * Helpers
  , screenRect, emptyWindowInfo, All (..)
  ) where

import Control.Concurrent.Chan (Chan)
import qualified Control.Exception as E
import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import Data.Monoid (All (..), Endo (..))
import qualified Data.Set as S
import Data.Typeable (Typeable, cast)
import System.Exit (ExitCode)
import System.IO (hPutStrLn, stderr)

import Rivermonad.Keysym
import qualified Rivermonad.StackSet as W
import Rivermonad.StackSet (StackSet, Stack (..))
import Rivermonad.Wayland.Protocol (Event)
import Rivermonad.Wayland.Wire (Conn, ObjectId)

-- | A window is identified by its river_window_v1 object.
type Window = ObjectId
type WorkspaceId = String

newtype ScreenId = S Int
  deriving (Eq, Ord, Show, Read, Enum, Num, Integral, Real)

data Rectangle = Rectangle
  { rect_x      :: !Int
  , rect_y      :: !Int
  , rect_width  :: !Int
  , rect_height :: !Int
  } deriving (Show, Read, Eq, Ord)

data ScreenDetail = SD
  { sdOutput :: !ObjectId   -- ^ the river_output_v1 behind this screen
  , sdRect   :: !Rectangle  -- ^ area available to windows (bars excluded)
  } deriving (Eq, Show)

type WindowSet = StackSet WorkspaceId (Layout Window) Window ScreenId ScreenDetail
type WindowSpace = W.Workspace WorkspaceId (Layout Window) Window

screenRect :: ScreenDetail -> Rectangle
screenRect = sdRect

------------------------------------------------------------------------------
-- Messages and layouts

class Typeable a => Message a

data SomeMessage = forall a. Message a => SomeMessage a

fromMessage :: Message m => SomeMessage -> Maybe m
fromMessage (SomeMessage m) = cast m

-- | The layout class, as in xmonad. Implement 'pureLayout' / 'pureMessage'
-- for simple layouts, or 'doLayout' / 'handleMessage' when you need 'X'.
class Show (layout a) => LayoutClass layout a where
  -- | Lay out a whole workspace. Rarely overridden.
  runLayout :: W.Workspace WorkspaceId (layout a) a -> Rectangle -> X ([(a, Rectangle)], Maybe (layout a))
  runLayout (W.Workspace _ l ms) r = maybe (emptyLayout l r) (doLayout l r) ms

  doLayout :: layout a -> Rectangle -> Stack a -> X ([(a, Rectangle)], Maybe (layout a))
  doLayout l r s = pure (pureLayout l r s, Nothing)

  pureLayout :: layout a -> Rectangle -> Stack a -> [(a, Rectangle)]
  pureLayout _ r s = [(W.focus s, r)]

  emptyLayout :: layout a -> Rectangle -> X ([(a, Rectangle)], Maybe (layout a))
  emptyLayout _ _ = pure ([], Nothing)

  -- | React to a message. 'Nothing' means the layout did not change.
  handleMessage :: layout a -> SomeMessage -> X (Maybe (layout a))
  handleMessage l = pure . pureMessage l

  pureMessage :: layout a -> SomeMessage -> Maybe (layout a)
  pureMessage _ _ = Nothing

  -- | Human readable name, used by 'JumpToLayout' and status bars.
  description :: layout a -> String
  description = takeWhile (/= ' ') . show

-- | A layout of unknown type. 'Read' is required so layouts survive restarts.
data Layout a = forall l. (LayoutClass l a, Read (l a)) => Layout (l a)

instance Show (Layout a) where
  show (Layout l) = show l

instance LayoutClass Layout a where
  runLayout (W.Workspace i (Layout l) ms) r = fmap (fmap Layout) <$> runLayout (W.Workspace i l ms) r
  doLayout (Layout l) r s = fmap (fmap Layout) <$> doLayout l r s
  emptyLayout (Layout l) r = fmap (fmap Layout) <$> emptyLayout l r
  handleMessage (Layout l) m = fmap Layout <$> handleMessage l m
  description (Layout l) = description l

-- | Parse a layout using another layout of the same type as a template.
readsLayout :: Layout a -> String -> [(Layout a, String)]
readsLayout (Layout l) s = [ (Layout (x `asTypeOf` l), rest) | (x, rest) <- reads s ]

------------------------------------------------------------------------------
-- Extensible state (see Rivermonad.Util.ExtensibleState)

class Typeable a => ExtensionClass a where
  initialValue :: a
  -- | Use 'PersistentExtension' to keep the value across restarts.
  extensionType :: a -> StateExtension
  extensionType = StateExtension

data StateExtension
  = forall a. ExtensionClass a => StateExtension a
  | forall a. (Read a, Show a, ExtensionClass a) => PersistentExtension a

------------------------------------------------------------------------------
-- Per-window state

-- | Everything we know about a window from the compositor.
data WindowInfo = WindowInfo
  { wiNode       :: !ObjectId
  , wiAppId      :: !String
  , wiTitle      :: !String
  , wiIdentifier :: !String
  , wiParent     :: !(Maybe Window)
  , wiMinSize    :: !(Int, Int)
  , wiMaxSize    :: !(Int, Int)        -- ^ 0 = no preference
  , wiDims       :: !(Maybe (Int, Int)) -- ^ actual size reported by the compositor
  , wiDecoration :: !Word
  , wiManaged    :: !Bool               -- ^ already went through the manage hook
  , wiProposed   :: !(Maybe (Int, Int)) -- ^ last proposed size, to avoid resending
  , wiTiled      :: !(Maybe Word)       -- ^ last set_tiled edges sent
  , wiFullscreen :: !(Maybe ObjectId)   -- ^ output it is currently fullscreen on
  , wiDecoSent   :: !(Maybe Bool)       -- ^ last use_ssd (True) / use_csd (False) sent
  , wiGeom       :: !(Maybe Rectangle)  -- ^ last placement, borders included
  } deriving (Show)

emptyWindowInfo :: ObjectId -> WindowInfo
emptyWindowInfo node = WindowInfo
  { wiNode = node, wiAppId = "", wiTitle = "", wiIdentifier = "", wiParent = Nothing
  , wiMinSize = (0, 0), wiMaxSize = (0, 0), wiDims = Nothing, wiDecoration = 3
  , wiManaged = False, wiProposed = Nothing, wiTiled = Nothing, wiFullscreen = Nothing
  , wiDecoSent = Nothing, wiGeom = Nothing }

data OutputInfo = OutputInfo
  { oiPos        :: !(Maybe (Int, Int))
  , oiDims       :: !(Maybe (Int, Int))
  , oiUsable     :: !(Maybe Rectangle)  -- ^ from river_layer_shell_output_v1
  , oiLayerShell :: !(Maybe ObjectId)
  } deriving (Show)

data DragKind = DragMove | DragResize deriving (Show, Eq)

data Drag = Drag
  { dragKind   :: !DragKind
  , dragWindow :: !Window
  , dragStart  :: !Rectangle   -- ^ absolute geometry when the drag started
  , dragDelta  :: !(Int, Int)
  , dragSeat   :: !ObjectId
  } deriving (Show)

data SeatInfo = SeatInfo
  { siBindings      :: ![ObjectId]          -- ^ xkb and pointer binding objects
  , siEnabled       :: !Bool                -- ^ bindings enabled already
  , siPointerWindow :: !(Maybe Window)
  , siPointerPos    :: !(Maybe (Int, Int))
  , siPosAtLayout   :: !(Maybe (Int, Int))  -- ^ pointer position at the last layout change
  , siFocused       :: !(Maybe (Maybe Window)) -- ^ last focus request sent (Nothing = unknown)
  , siLayerSeat     :: !(Maybe ObjectId)
  , siLayerFocus    :: !Bool                -- ^ a layer surface (launcher, bar) has focus
  } deriving (Show)

data Globals = Globals
  { gWm         :: !ObjectId
  , gWmVersion  :: !Int
  , gXkb        :: !ObjectId
  , gLayerShell :: !(Maybe ObjectId)
  }

-- | Thrown to unwind the main loop so the new binary can be exec'd.
data RestartNow = RestartNow deriving (Show)
instance E.Exception RestartNow

-- | Things the main loop reacts to.
data Input
  = InEvents [Event]
  | InDisconnected String
  | InRestart     -- ^ restart the window manager (SIGUSR1 or after recompiling)
  | InQuit        -- ^ SIGTERM / SIGINT

------------------------------------------------------------------------------
-- Configuration

class Default a where
  def :: a

data XConfig = XConfig
  { terminal           :: !String
  , modMask            :: !KeyMask
  , borderWidth        :: !Int
  , normalBorderColor  :: !String              -- ^ "#rrggbb" or "#rrggbbaa"
  , focusedBorderColor :: !String
  , workspaces         :: ![WorkspaceId]
  , layoutHook         :: !(Layout Window)
  , manageHook         :: !ManageHook
  , handleEventHook    :: !(Event -> X All)   -- ^ return @All False@ to skip default handling
  , keys               :: !(XConfig -> M.Map (KeyMask, KeySym) (X ()))
  , mouseBindings      :: !(XConfig -> M.Map (KeyMask, Button) (Window -> X ()))
  , startupHook        :: !(X ())
  , logHook            :: !(X ())
  , focusFollowsMouse  :: !Bool
  , xcursorTheme       :: !(Maybe (String, Int))
  }

------------------------------------------------------------------------------
-- The X monad

data XConf = XConf
  { config  :: !XConfig
  , conn    :: !Conn
  , globals :: !Globals
  , inputs  :: !(Chan Input)
  }

data XState = XState
  { windowset       :: !WindowSet
  , windowInfo      :: !(M.Map Window WindowInfo)
  , outputs         :: !(M.Map ObjectId OutputInfo)
  , seats           :: !(M.Map ObjectId SeatInfo)
  , keyActions      :: !(M.Map ObjectId (X ()))
  , pointerActions  :: !(M.Map ObjectId (ObjectId, Window -> X ())) -- ^ binding -> (seat, action)
  , pending         :: ![X ()]          -- ^ actions for the next manage sequence (newest first)
  , newWindows      :: ![Window]        -- ^ windows awaiting the manage hook (newest first)
  , outputsDirty    :: !Bool
  , haveScreens     :: !Bool
  , fullscreen      :: !(S.Set Window)
  , needsCenter     :: !(S.Set Window)  -- ^ floats to center once their size is known
  , drag            :: !(Maybe Drag)
  , sessionLocked   :: !Bool
  , started         :: !Bool            -- ^ startup hook already ran
  , restoreState    :: !(M.Map String (WorkspaceId, Maybe RationalRectS))
  , restarting      :: !Bool
  , restoreView     :: !(Maybe WorkspaceId) -- ^ workspace to show after a restart
  , restoreOrder    :: !(M.Map String (Int, Bool)) -- ^ identifier -> (stack position, focused)
  , borderWidths    :: !(M.Map Window Int)  -- ^ per-window border overrides set by layouts
  , extensibleState :: !(M.Map String (Either String StateExtension))
  }

-- | Serializable float geometry used by the restart state file.
type RationalRectS = (Rational, Rational, Rational, Rational)

newtype X a = X (ReaderT XConf (StateT XState IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadState XState, MonadReader XConf)

instance Semigroup a => Semigroup (X a) where
  a <> b = (<>) <$> a <*> b

instance Monoid a => Monoid (X a) where
  mempty = pure mempty

runX :: XConf -> XState -> X a -> IO (a, XState)
runX c st (X m) = runStateT (runReaderT m c) st

io :: MonadIO m => IO a -> m a
io = liftIO

-- | Run an action, falling back to another one (and discarding state
-- changes) if it throws. 'ExitCode' and restart exceptions are re-thrown.
catchX :: X a -> X a -> X a
catchX job errcase = do
  st <- get
  c <- ask
  (a, s') <- io $ runX c st job `E.catch` \(e :: E.SomeException) ->
    case (E.fromException e, E.fromException e) of
      (Just (_ :: ExitCode), _) -> E.throwIO e
      (_, Just RestartNow) -> E.throwIO e
      _ -> do
        hPutStrLn stderr ("rivermonad: user code raised: " ++ show e)
        runX c st errcase
  put s'
  pure a

userCode :: X a -> X (Maybe a)
userCode a = catchX (Just <$> a) (pure Nothing)

userCodeDef :: a -> X a -> X a
userCodeDef d a = maybe d id <$> userCode a

withWindowSet :: (WindowSet -> X a) -> X a
withWindowSet f = gets windowset >>= f

------------------------------------------------------------------------------
-- Queries

-- | A computation that inspects a window, used by manage hooks.
newtype Query a = Query (ReaderT Window X a)
  deriving (Functor, Applicative, Monad, MonadReader Window, MonadIO)

instance Semigroup a => Semigroup (Query a) where
  a <> b = (<>) <$> a <*> b

instance Monoid a => Monoid (Query a) where
  mempty = pure mempty

type ManageHook = Query (Endo WindowSet)

runQuery :: Query a -> Window -> X a
runQuery (Query m) = runReaderT m

liftX :: X a -> Query a
liftX = Query . lift
