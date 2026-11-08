-- | Small self-contained checks for the pure parts (no QuickCheck needed).
module Main (main) where

import Control.Monad (unless)
import Data.List (nub, sort)
import qualified Data.Map as M
import System.Exit (exitFailure)

import Rivermonad.Layout
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
    , check "mirrorRect is an involution" (mirrorRect (mirrorRect (Rectangle 1 2 3 4)) == Rectangle 1 2 3 4)
    ]
  unless (and rs) exitFailure
  where
    readsTall :: String -> Maybe (Tall Int)
    readsTall str = case reads str of
      [(x, "")] -> Just x
      _ -> Nothing
