-- Example ~/.config/rivermonad/rivermonad.hs
--
-- Recompile with `rivermonad --recompile` or just press mod-q.
import Rivermonad
import qualified Rivermonad.StackSet as W

import Rivermonad.Actions.CycleWS
import Rivermonad.Hooks.InsertPosition
import Rivermonad.Hooks.ManageHelpers
import Rivermonad.Hooks.StatusBar
import Rivermonad.Layout.NoBorders
import Rivermonad.Layout.Spacing
import Rivermonad.Layout.ThreeColumns
import Rivermonad.Layout.ToggleLayouts
import Rivermonad.Util.EZConfig
import Rivermonad.Util.NamedScratchpad
import Rivermonad.Util.SpawnOnce

main :: IO ()
main = rivermonad . withStatusBar myPP $ myConfig `additionalKeysP` myKeys

myConfig :: XConfig
myConfig = def
  { terminal           = "foot"
  , modMask            = mod4Mask            -- Super; use mod1Mask for Alt
  , borderWidth        = 2
  , normalBorderColor  = "#3b4252"
  , focusedBorderColor = "#88c0d0"
  , workspaces         = ["web", "code", "term"] ++ map show [4 .. 9 :: Int]
  , layoutHook         = myLayout
  , manageHook         = myManageHook
  , startupHook        = spawnOnce "waybar"
  }

myLayout :: Layout Window
myLayout = Layout $ toggleLayouts (noBorders Full) $ smartBorders $ spacingWithEdge 4 $
  tiled ||| Mirror tiled ||| ThreeColMid 1 (3 / 100) (1 / 2)
  where
    tiled = Tall 1 (3 / 100) (3 / 5)

scratchpads :: NamedScratchpads
scratchpads =
  [ NS "term" "foot --app-id scratchpad" (appId =? "scratchpad")
       (customFloating (W.RationalRect 0.1 0.1 0.8 0.6)) ]

myManageHook :: ManageHook
myManageHook = insertPosition Below Newer <> namedScratchpadManageHook scratchpads <> composeOne
  [ isDialog                       -?> doCenterFloat
  , appId =? "pavucontrol"         -?> doCenterFloat
  , appId =? "firefox"             -?> doShift "web"
  , title =? "Picture-in-Picture"  -?> doFloat
  ]

myKeys :: [(String, X ())]
myKeys =
  [ ("M-b",         spawn "firefox")
  , ("M-z",         sendMessage ToggleLayout)
  , ("M-`",         namedScratchpadAction scratchpads "term")
  , ("M-<Right>",   nextWS)
  , ("M-<Left>",    prevWS)
  , ("M-<Tab>",     toggleWS)
  , ("M-S-f",       withFocused float)
  , ("M-=",         incSpacing 2)
  , ("M--",         incSpacing (-2))
  , ("<Print>",     spawn "grim -g \"$(slurp)\" - | wl-copy")
  , ("<XF86AudioRaiseVolume>", spawn "wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+")
  , ("<XF86AudioLowerVolume>", spawn "wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-")
  , ("<XF86AudioMute>",        spawn "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle")
  ]

myPP :: PP
myPP = def
  { ppCurrent = pangoColor "#88c0d0" . wrap "[" "]"
  , ppHidden  = id
  , ppTitle   = pangoColor "#d8dee9"
  }
