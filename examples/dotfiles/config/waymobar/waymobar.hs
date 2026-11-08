{-# LANGUAGE LambdaCase #-}
-- ~/.config/waymobar/waymobar.hs
--
-- waymobar rebuilds this file when it changes; check it with
-- `waymobar --recompile`. Reload: `pkill -x waymobar-aarch6; waymobar &`
-- Docs: https://github.com/prietus/waymobar
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)

import Waymobar

-- Nord, like the rivermonad borders
nord0, nord1, nord2, nord3, nord4, nord8, nord9, nord11, nord13 :: String
nord0 = "#2e3440"; nord1 = "#3b4252"; nord2 = "#434c5e"; nord3 = "#4c566a"
nord4 = "#d8dee9"; nord8 = "#88c0d0"; nord9 = "#81a1c1"; nord11 = "#bf616a"
nord13 = "#ebcb8b"

------------------------------------------------------------------------------
-- Pill theme: a transparent bar where every widget is a rounded "island".

-- | The look of every pill.
pill :: Style
pill = noStyle
  { styleBg      = Just nord0
  , styleBorder  = Just (1, nord2)
  , styleRadius  = Just 12
  , styleMargin  = Just (3, 0)       -- (vertical, horizontal): 3 px above and below
  , stylePadding = Just 10           -- room between the text and the rounded ends
  }

-- | Pills that react to the pointer (the clickable ones).
clickablePill :: Style
clickablePill = pill { styleHover = Just noStyle { styleBg = Just nord1, styleBorder = Just (1, nord3) } }

-- | Put a widget in its own pill. 'baseStyle' keeps the widget's own colours.
inPill, inClickablePill :: Widget -> Widget
inPill = baseStyle pill
inClickablePill = baseStyle clickablePill

-- | The clock: an accent-coloured pill.
accentPill :: Style
accentPill = pill { styleBg = Just nord8, styleFg = Just nord0, styleBorder = Nothing }

main :: IO ()
main = waymobar def
  { font = "monospace 10"
  , barHeight = 30
  , margins = (0, 1, 0, 1)   -- same 1 px side gap as rivermonad's gapsWithEdge 1
  , bgColor = "#00000000"    -- transparent: only the pills are drawn
  , fgColor = nord4
  , widgetSpacing = 6
  , edgePadding = 2
  , left =
      [ myWorkspaces
      , inClickablePill (colored nord9 (layoutName shortLayout))
      ]
  , center = [ inPill (windowTitle 80) ]
  , right =
      [ inClickablePill media
      , inPill net
      , inClickablePill myVolume
      , inPill (cpu (\p -> icCpu ++ warn 80 p (show p ++ "%")))
      , inPill (memory (\p -> icMem ++ warn 85 p (show p ++ "%")))
      , inPill (battery (\p st -> batteryIcon p st ++ warn' (p <= 15 && st /= "Charging") (show p ++ "%")))
      -- one pill around all the tray icons
      , groupStyle pill { stylePadding = Just 6 } (tray defaultTray { traySymbolicColor = nord4 })
      , withStyle accentPill (clock (icDate ++ "%a %d %b  " ++ icClock ++ "%H:%M"))
        -- log out: asks to confirm, closes every window, then back to GDM
      , inClickablePill (colored nord11 (logoutButton "\xf011"))   -- nf-fa-power_off
      ]
  }

-- Nerd Font icons (the "Symbols Nerd Font" in ~/.local/share/fonts; any
-- glyph from https://www.nerdfonts.com/cheat-sheet works). Each ends with
-- a space to separate it from the text.
icCpu, icMem, icDate, icClock, icBolt :: String
icCpu   = "\xf4bc "   -- nf-oct-cpu
icMem   = "\xefc5 "   -- nf-fa-memory
icDate  = "\xf073 "   -- nf-fa-calendar
icClock = "\xf017 "   -- nf-fa-clock
icBolt  = "\xf0e7"    -- nf-fa-bolt

-- Battery level icon (nf-fa-battery_*), with a bolt when charging.
batteryIcon :: Int -> String -> String
batteryIcon p st = (if st == "Charging" then icBolt else "") ++ level ++ " "
  where
    level | p >= 90 = "\xf240" | p >= 60 = "\xf241" | p >= 35 = "\xf242"
          | p >= 10 = "\xf243" | otherwise = "\xf244"

-- Icons in front of the named workspaces.
wsIcon :: String -> String
wsIcon "web"  = "\xf269 "   -- nf-fa-firefox
wsIcon "code" = "\xf121 "   -- nf-fa-code
wsIcon "term" = "\xf120 "   -- nf-fa-terminal
wsIcon _      = ""

-- web/code/term always; numbered workspaces only when they have windows or
-- are on screen (a block with an empty label is not drawn).
myWorkspaces :: Widget
myWorkspaces = workspaces defaultWorkspaces
  { wsLabel = \w ->
      if wsName w `elem` ["web", "code", "term"] || wsWindows w > 0 || wsVisible w
        then wsIcon (wsName w) ++ escape (wsName w)
        else ""
  , wsPadding = 8
    -- each workspace is a small pill inside the big one
  , focusedStyle = wsPill { styleFg = Just nord0, styleBg = Just nord8 }
  , activeStyle  = wsPill { styleBg = Just nord2 }                       -- on this monitor
  , visibleStyle = wsHover { styleFg = Just nord13 }                     -- on the other monitor
  , hiddenStyle  = wsHover
  , emptyStyle   = wsHover { styleFg = Just nord3 }
  }
  & groupStyle pill { stylePadding = Just 3 }   -- the big pill around them
  where
    wsPill = noStyle { styleRadius = Just 9, styleMargin = Just (6, 1) }
    wsHover = wsPill { styleHover = Just noStyle { styleBg = Just nord1 } }
    w & f = f w

-- "Spacing Tall" -> "Tall", "Mirror Tall" stays.
shortLayout :: String -> String
shortLayout l = "[" ++ fromMaybe l (stripPrefix "Spacing " l) ++ "]"

-- Volume of the default output, live through pipewire-pulse: click mutes,
-- the wheel changes it (up to 100%).
myVolume :: Widget
myVolume = volume $ \case
  Nothing -> ""
  Just v
    | volMuted v -> "<span foreground='" ++ nord3 ++ "'>\xf026 muted</span>"   -- nf-fa-volume_off
    | otherwise  -> (if volPercent v < 50 then "\xf027 " else "\xf028 ") ++ show (volPercent v) ++ "%"

-- Network: Wi-Fi name and signal, or the wired IP address.
net :: Widget
net = network 3 $ \case
  Nothing -> "<span foreground='" ++ nord3 ++ "'>\xf127 offline</span>"           -- nf-fa-chain_broken
  Just n -> case netKind n of
    Wifi -> "\xf1eb " ++ maybe "wifi" escape (netName n)                         -- nf-fa-wifi
              ++ maybe "" (\s -> " " ++ show s ++ "%") (netSignal n)
    _    -> "\xf0e8 " ++ maybe (netInterface n) id (netAddress n)                -- nf-fa-sitemap

-- The current media player: left click play/pause, right click next,
-- middle click previous. Hidden when nothing is playing or paused.
media :: Widget
media = colored nord9 $ mpris $ \m ->
  if mediaStatus m == Stopped then "" else
    (if mediaStatus m == Playing then "\xf04b " else "\xf04c ")             -- nf-fa-play / pause
      ++ escape (shorten 40 (if null (mediaArtist m) then mediaTitle m else mediaArtist m ++ " - " ++ mediaTitle m))

warn :: Int -> Int -> String -> String
warn limit p = warn' (p >= limit)

warn' :: Bool -> String -> String
warn' True s = "<span foreground='" ++ nord11 ++ "'>" ++ s ++ "</span>"
warn' False s = s
