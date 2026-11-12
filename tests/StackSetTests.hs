-- | Small self-contained checks for the pure parts (no QuickCheck needed).
module Main (main) where

import Control.Monad (unless)
import Data.List (nub, sort)
import qualified Data.Map as M
import System.Exit (exitFailure)

import Rivermonad.Actions.DynamicWorkspaces (Plan (..), addHidden, plan, removeEmpty, renameTag')
import Rivermonad.Config (defaultConfig)
import Rivermonad.Core (IPCAction (..), Window, readsLayout)
import Rivermonad.Hooks.RefocusLast (refocusLastLayoutHook)
import Rivermonad.IPC (parseAction)
import qualified Rivermonad.JSON as J
import Rivermonad.Keysym
import Rivermonad.Layout
import Rivermonad.Layout.FocusTracking (focusOn, focusTracking)
import Rivermonad.Layout.Gaps (Direction2D (..), applyGaps, gaps)
import Rivermonad.Layout.Grid (arrange)
import Rivermonad.Layout.NoBorders (noBorders)
import Rivermonad.Layout.LayoutModifier (LayoutModifier (..))
import Rivermonad.Layout.Spacing (Spacing (..))
import Rivermonad.Layout.ResizableTile (ResizableTall (..))
import Rivermonad.Layout.ThreeColumns (ThreeCol (..))
import Rivermonad.Util.EZConfig (readKey)
import qualified Rivermonad.StackSet as W

type T = W.StackSet String () Int Int ()

fresh :: T
fresh = W.new () ["1", "2", "3"] [(), ()]

check :: String -> Bool -> IO Bool
check name ok = do
  putStrLn ((if ok then "ok    " else "FAIL  ") ++ name)
  pure ok

invariant :: T -> Bool
invariant s = let ws = concatMap (W.integrate' . W.stack) (W.workspaces s)
              in ws == nub ws && sort (map W.tag (W.workspaces s)) == ["1", "2", "3"]

main :: IO ()
main = do
  let s1 = foldr W.insertUp fresh [1 .. 5]
      s2 = W.shift "2" s1
      s3 = W.view "2" s2
      s4 = W.greedyView "3" s3
      s5 = W.delete 3 (W.float 3 (W.RationalRect 0 0 1 1) s1)
      tiles = tile (1 / 2) (Rectangle 0 0 1000 800) 1 3
      full = maybe [] (pureLayout (Tall 1 (3 / 100) (1 / 2) :: Tall Int) (Rectangle 0 0 100 100))
                       (W.differentiate [1, 2, 3 :: Int])
      shrunk = pureMessage (Tall 1 (1 / 10) (1 / 2) :: Tall Int) (SomeMessage Shrink)
      mirrored = maybe [] (pureLayout (Tall 1 (3 / 100) (1 / 2) :: Tall Int) (Rectangle 0 0 100 100))
                       (W.differentiate [1, 2 :: Int])
  rs <- sequence
    [ check "insertUp focuses the new window" (W.peek s1 == Just 1)
    , check "insertUp keeps windows unique" (invariant s1 && length (W.index s1) == 5)
    , check "shift moves the focused window" (W.findTag 1 s2 == Just "2" && W.peek s2 /= Just 1)
    , check "view switches screens for visible workspaces" (W.currentTag s3 == "2" && W.screen (W.current s3) == 1)
    , check "greedyView pulls hidden workspace" (W.currentTag s4 == "3" && invariant s4)
    , check "focusWindow finds windows on other workspaces" (W.peek (W.focusWindow 1 s2) == Just 1)
    , check "focusUp/focusDown are inverse" (W.focusUp (W.focusDown s1) == s1)
    , check "swapMaster puts focus first" (let s = W.swapMaster (W.focusDown s1) in take 1 (W.index s) == maybe [] pure (W.peek s))
    , check "delete removes window and float" (not (W.member 3 s5) && M.notMember 3 (W.floating s5))
    , check "tile: master takes half" (map rect_width tiles == [500, 500, 500] && map rect_height tiles == [800, 400, 400])
    , check "Tall places every window" (length full == 3)
    , check "Shrink reduces the master ratio" (fmap tallRatio shrunk == Just (2 / 5))
    , check "Tall splits two windows side by side" (map (rect_x . snd) mirrored == [0, 50])
    , check "layouts round-trip through Read" (fmap show (readsTall (show (Tall 2 (1 / 10) (1 / 3) :: Tall Int))) == Just (show (Tall 2 (1 / 10) (1 / 3) :: Tall Int)))
    , check "EZConfig: M-S-<Return>" (readKey defaultConfig "M-S-<Return>" == Just (mod4Mask + shiftMask, xK_Return))
    , check "EZConfig: C-M1-x and <XF86AudioMute>" (readKey defaultConfig "C-M1-x" == Just (controlMask + mod1Mask, xK_x)
                                                    && readKey defaultConfig "<XF86AudioMute>" == Just (0, xK_XF86AudioMute))
    , check "EZConfig: aliases and errors" (readKey defaultConfig "M-<Esc>" == Just (mod4Mask, xK_Escape)
                                            && readKey defaultConfig "M-<Nope>" == Nothing && readKey defaultConfig "M-" == Nothing)
    , check "Grid covers the screen without overlap" (gridOk 5 && gridOk 7 && gridOk 1)
    , check "ThreeColMid puts the master in the middle" (threeMid == [250, 750, 750, 0])
    , check "ResizableTall honours relative heights" (map (rect_height . snd) resizable == [800, 200, 600])
    , check "gapsWithEdge 1: one pixel between windows and to every edge" gapsOk
    , check "mirrorRect is an involution" (mirrorRect (mirrorRect (Rectangle 1 2 3 4)) == Rectangle 1 2 3 4)
    , check "EZConfig: modifier keys are keys" (readKey defaultConfig "<Super_R>" == Just (0, xK_Super_R)
                                               && readKey defaultConfig "C-S-<Super_R>" == Just (controlMask + shiftMask, xK_Super_R))
    , check "Gaps: one size per edge" (applyGaps [(U, 10), (R, 20), (D, 30), (L, 40)] area == Rectangle 40 10 940 760)
    , check "Gaps: never below one pixel" (let Rectangle _ _ w h = applyGaps [(L, 5000), (R, 5000), (U, 5000)] area in w == 1 && h == 1)
    , check "focusOn keeps the order" (fmap W.integrate (W.differentiate [1, 2, 3 :: Int] >>= focusOn 3) == Just [1, 2, 3]
                                       && fmap W.focus (W.differentiate [1, 2, 3 :: Int] >>= focusOn 3) == Just 3
                                       && (W.differentiate [1, 2 :: Int] >>= focusOn 9) == Nothing)
    , check "a layout with all modifiers round-trips through Read" modifiersRoundTrip
    , check "addHidden appends once" (map W.tag (W.workspaces (addHidden () "x" (addHidden () "x" fresh))) == ["1", "2", "3", "x"])
    , check "removeEmpty drops a hidden workspace" (fmap (map W.tag . W.workspaces) (removeEmpty "3" fresh) == Just ["1", "2"])
    , check "removeEmpty replaces a workspace that is on screen" (fmap W.currentTag (removeEmpty "1" fresh) == Just "3"
                                                                 && fmap invariant2 (removeEmpty "1" fresh) == Just True)
    , check "removeEmpty needs a replacement for a workspace on screen" (removeEmpty "1" two == Nothing)
    , check "removeEmpty keeps workspaces with windows" (removeEmpty "1" s1 == Nothing && removeEmpty "nope" fresh == Nothing)
    , check "renameTag' renames" (map W.tag (W.workspaces (renameTag' "2" "web" fresh)) == ["1", "web", "3"])
    , check "plan: a name replaced in place is a rename" (plan ["1", "2", "3"] ["1", "web", "3"] == Plan [("2", "web")] [] [])
    , check "plan: additions and removals" (plan ["1", "2", "3"] ["1", "3", "mail"] == Plan [] ["mail"] ["2"])
    , check "plan: reordering changes nothing" (plan ["1", "2", "3"] ["3", "1", "2"] == Plan [] [] [])
    , check "plan: rename and removal together" (plan ["1", "www", "mail", "chat", "3"] ["1", "web", "chat", "3"] == Plan [("www", "web")] [] ["mail"])
    , check "IPC: workspace and window actions parse" ipcOk
    ]
  unless (and rs) exitFailure
  where
    area = Rectangle 0 0 1000 800
    two = W.new () ["1", "2"] [(), ()] :: T
    invariant2 s = sort (map W.tag (W.workspaces s)) == ["2", "3"]
    modifiersRoundTrip =
      let l = Layout (refocusLastLayoutHook (gaps [(U, 20), (L, 3)] (ResizableTall 1 (1 / 20) (1 / 2) [] ||| noBorders (focusTracking Full)))) :: Layout Window
      in case readsLayout l (show l) of
           [(l', rest)] -> all (== ' ') rest && show l' == show l
           _ -> False
    act str = J.parse str >>= parseAction
    ipcOk = act "{\"ShiftWindow\":[7,\"mail\"]}" == Just (IPCShiftWindow 7 "mail")
         && act "{\"ShiftWindow\":[7.5,\"mail\"]}" == Nothing
         && act "{\"FocusWindow\":4278190080}" == Just (IPCFocusWindow 4278190080)
         && act "{\"SetWorkspaces\":[\"1\",\"web\"]}" == Just (IPCSetWorkspaces ["1", "web"])
         && act "{\"SetWorkspaces\":[\"1\",2]}" == Nothing
         && act "{\"RenameWorkspace\":[\"2\",\"web\"]}" == Just (IPCRenameWorkspace "2" "web")
         && act "{\"AddWorkspace\":\"mail\"}" == Just (IPCAddWorkspace "mail")
         && act "{\"RemoveWorkspace\":\"mail\"}" == Just (IPCRemoveWorkspace "mail")
         && act "\"Quit\"" == Just IPCQuit
    -- gapsWithEdge 1 on two Tall windows: the screen is inset (1,1,0,0),
    -- every window (0,0,1,1).
    gapsOk =
      let sp = Spacing 1 True False True True :: Spacing Int
          screen' = Rectangle 1 1 999 799
          tiled = maybe [] (pureLayout (Tall 1 (3 / 100) (1 / 2) :: Tall Int) screen') (W.differentiate [1, 2 :: Int])
          rs = map snd (fst (pureModifier sp area (W.differentiate [1, 2]) tiled))
      in case rs of
           [a, b] -> rect_x a == 1 && rect_y a == 1                                -- left/top edge: 1 px
                     && rect_x b - (rect_x a + rect_width a) == 1                  -- between: 1 px
                     && 1000 - (rect_x b + rect_width b) == 1                      -- right edge: 1 px
                     && 800 - (rect_y a + rect_height a) == 1                      -- bottom edge: 1 px
           _ -> False
    gridOk n = let rs' = map snd (arrange 1 area [1 .. n :: Int])
               in length rs' == n && sum [ rect_width r * rect_height r | r <- rs' ] == 1000 * 800
    threeMid = map (rect_x . snd) (maybe [] (pureLayout (ThreeColMid 1 (3 / 100) (1 / 2) :: ThreeCol Int) area)
                                     (W.differentiate [1, 2, 3, 4 :: Int]))
    resizable = maybe [] (pureLayout (ResizableTall 1 (3 / 100) (1 / 2) [1, 1, 3] :: ResizableTall Int) area)
                  (W.differentiate [1, 2, 3 :: Int])
    readsTall :: String -> Maybe (Tall Int)
    readsTall str = case reads str of
      [(x, "")] -> Just x
      _ -> Nothing
