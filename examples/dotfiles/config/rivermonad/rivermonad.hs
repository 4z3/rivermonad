-- ~/.config/rivermonad/rivermonad.hs (see examples/dotfiles/README.md)
--
-- Recompile with `rivermonad --recompile` or just press mod-q.
import Rivermonad
import qualified Rivermonad.StackSet as W

import Rivermonad.Actions.CycleWS
import Rivermonad.Hooks.InsertPosition
import Rivermonad.Hooks.ManageHelpers
import Rivermonad.Layout.NoBorders
import Rivermonad.Layout.Spacing
import Rivermonad.Layout.ThreeColumns
import Rivermonad.Layout.ToggleLayouts
import Rivermonad.Util.EZConfig
import Rivermonad.Util.NamedScratchpad
import Rivermonad.Util.SpawnOnce

main :: IO ()
main = rivermonad $ myConfig `additionalKeysP` myKeys

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
    -- US keys plus Spanish with AltGr (the right Option key on a Mac):
    -- AltGr+n = ñ, AltGr+' then a vowel = á é í ó ú, AltGr+? = ¿, AltGr+1 = ¡
  , input              = def { keyboard = def { kbLayout = "us", kbVariant = "altgr-intl" } }
  , startupHook        = do
      spawnOnce "waymobar"
      spawnOnce "mako"
      -- wallpaper: ~/.config/wallpaper is a link to the image; change it with
      --   ln -sfn /path/to/image ~/.config/wallpaper; pkill -x swaybg; setsid -f swaybg -m fill -i ~/.config/wallpaper
      spawnOnce "swaybg -m fill -i ~/.config/wallpaper"
  }

myLayout :: Layout Window
myLayout = Layout $ toggleLayouts (noBorders Full) $ smartBorders $ gapsWithEdge 1 $
  tiled ||| Mirror tiled ||| ThreeColMid 1 (3 / 100) (1 / 2)
  where
    tiled = Tall 1 (3 / 100) (3 / 5)

scratchpads :: NamedScratchpads
scratchpads =
  [ NS "term" "foot --app-id scratchpad" (appId =? "scratchpad")
       (customFloating (W.RationalRect 0.1 0.1 0.8 0.6)) ]

myManageHook :: ManageHook
myManageHook = insertPosition Below Newer <> namedScratchpadManageHook scratchpads <> composeOne
  -- composeOne: the first matching rule wins, so floats go before doShift.
  -- Find a window's appId/title: focus it, then
  --   rivermonad --ipc '"EventStream"' | head -2
  [ isDialog                                 -?> doCenterFloat   -- windows with a parent
  , isFixedSize                              -?> doCenterFloat   -- min size = max size
  , appId =? "xdg-desktop-portal-gtk"        -?> doCenterFloat   -- open/save dialogs (snaps, portals)
  , appId =? "pavucontrol"                   -?> doCenterFloat
  , title =? "Picture-in-Picture"            -?> doFloat
    -- Firefox opens these as independent windows (no parent):
  , isFirefox <&&> (title =? "Library" <||> title ^? "Page Info" <||> title =? "About Mozilla Firefox")
                                             -?> doCenterFloat
  , isFirefox                                -?> doShift "web"
  ]
  where
    -- the snap reports "firefox_firefox", the .deb "firefox"
    isFirefox = appId =? "firefox_firefox" <||> appId =? "firefox"

myKeys :: [(String, X ())]
myKeys =
  [ ("M-b",         spawn "firefox")
  , ("M-z",         sendMessage ToggleLayout)
  , ("M-`",         namedScratchpadAction scratchpads "term")
  , ("M-<Right>",   nextWS)
  , ("M-<Left>",    prevWS)
  , ("M-<Tab>",     toggleWS)
  , ("M-S-f",       withFocused float)
  , ("M-=",         incSpacing 1)
  , ("M--",         incSpacing (-1))
    -- screenshots (~/Pictures/Screenshots + clipboard), see ~/.local/bin/shot
  , ("<Print>",     spawn "shot")              -- whole screen
  , ("S-<Print>",   spawn "shot region")       -- select a region
  , ("C-<Print>",   spawn "shot annotate")     -- region, then annotate in swappy
    -- screen recording (~/Videos/Recordings); same keys again to stop
  , ("M-<Print>",   spawn "screenrec")
  , ("M-S-<Print>", spawn "screenrec region")
    -- emoji picker: type "tree", Enter, and 🌳 is typed in the focused window
  , ("M-i",         spawn "emoji")
  , ("<XF86AudioRaiseVolume>", spawn "wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+")
  , ("<XF86AudioLowerVolume>", spawn "wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-")
  , ("<XF86AudioMute>",        spawn "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle")
  ]
