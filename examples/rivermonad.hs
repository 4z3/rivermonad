-- Example ~/.config/rivermonad/rivermonad.hs
--
-- Recompile with `rivermonad --recompile` or just press mod-q.
import Rivermonad
import qualified Rivermonad.StackSet as W
import qualified Data.Map as M

main :: IO ()
main = rivermonad def
  { terminal           = "foot"
  , modMask            = mod4Mask            -- Super; use mod1Mask for Alt
  , borderWidth        = 2
  , normalBorderColor  = "#3b4252"
  , focusedBorderColor = "#88c0d0"
  , workspaces         = ["web", "code", "term"] ++ map show [4 .. 9 :: Int]
  , layoutHook         = Layout (tiled ||| Mirror tiled ||| Full)
  , manageHook         = myManageHook
  , keys               = \c -> myKeys c `M.union` keys def c
  , startupHook        = spawn "waybar"
  }
  where
    tiled = Tall 1 (3 / 100) (3 / 5)

myManageHook :: ManageHook
myManageHook = composeAll
  [ appId =? "pavucontrol"     --> doCenterFloat
  , appId =? "firefox"         --> doShift "web"
  , title =? "Picture-in-Picture" --> doFloat
  , isDialog                   --> doFloat
  ]

myKeys :: XConfig -> M.Map (KeyMask, KeySym) (X ())
myKeys XConfig { modMask = m } = M.fromList
  [ ((m, xK_b), spawn "firefox")
  , ((0, xK_Print), spawn "grim -g \"$(slurp)\" - | wl-copy")
  , ((0, xK_XF86AudioRaiseVolume), spawn "wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+")
  , ((0, xK_XF86AudioLowerVolume), spawn "wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-")
  , ((0, xK_XF86AudioMute), spawn "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle")
  , ((m .|. shiftMask, xK_f), withFocused float)
  ]
