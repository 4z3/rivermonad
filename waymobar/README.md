# waymobar

A status bar for Wayland, written and configured in Haskell, in the spirit of
xmobar. It is the native bar for [rivermonad](../README.md): workspaces,
layout and window title come straight from rivermonad's IPC socket, and
clicking them acts on rivermonad.

* pure-Haskell Wayland client (`Waymobar.Wayland.Wire`, bindings generated
  from the XML in `protocol/`); text is drawn with cairo + pango through a
  small FFI, no GTK
* wlr-layer-shell surface on every output (follows hotplug), with an
  exclusive zone so tiled windows avoid it
* HiDPI: fractional scaling (`wp_fractional_scale_v1` + `wp_viewporter`), or
  integer buffer scale on compositors without it
* mouse: clicks and the wheel, cursor via `wp_cursor_shape_v1`
* system tray (StatusNotifierItem: nm-applet, Telegram, Steam, Discord,
  KDE and Qt apps, Electron, libappindicator...), with their menus shown as
  native popups; a small pure-Haskell D-Bus client, no libdbus
* popup menus for your own widgets too (`clickMenu`)
* reconnects by itself when rivermonad restarts (`mod-q`)
* works on any compositor with layer-shell (the rivermonad widgets just stay
  empty elsewhere)

## Build

```sh
sudo apt install libcairo2-dev libpango1.0-dev libgdk-pixbuf-2.0-dev   # plus ghc and cabal-install
cd waymobar
cabal install --lib waymobar      # so ghc can compile your config
cabal install exe:waymobar
```

Start it from your rivermonad config:

```haskell
startupHook = spawnOnce "waymobar"
```

## Configuration

Without a configuration `waymobar` shows the default bar: workspaces and
layout on the left, window title in the middle, CPU, memory, battery and
tray and clock on the right. To change it, write `~/.config/waymobar/waymobar.hs`
(see [examples/waymobar.hs](examples/waymobar.hs)):

```haskell
import Waymobar

main :: IO ()
main = waymobar def
  { font = "JetBrains Mono 10"
  , position = Bottom
  , left = [ workspaces defaultWorkspaces { hideEmpty = True } ]
  , right = [ cpu (\p -> "cpu " ++ show p ++ "%"), clock "%H:%M" ]
  }
```

Like xmonad, `waymobar` rebuilds the file when it changes and runs the
result; `waymobar --recompile` checks it. An executable
`~/.config/waymobar/build` script, if present, is used instead of ghc, with
the output path as `$1` (same convention as rivermonad, e.g. to build with
`cabal exec` against a checkout).

### `Config`

| Field | Default | |
|---|---|---|
| `font` | `"monospace 10"` | pango font description |
| `barHeight` | `0` | logical pixels; 0 = fit the font |
| `position` | `Top` | or `Bottom` |
| `layer` | `TopLayer` | `BackgroundLayer`, `BottomLayer`, `OverlayLayer` |
| `exclusive` | `True` | reserve the bar's space |
| `bgColor`, `fgColor` | Nord | `#rgb`, `#rrggbb`, `#rrggbbaa` |
| `left`, `center`, `right` | | lists of widgets |
| `widgetSpacing`, `edgePadding` | `12`, `4` | pixels |
| `outputs` | `[]` | only these outputs (`["DP-1"]`); `[]` = all |
| `namespace` | `"waymobar"` | layer-shell namespace |

### Widgets

| Widget | |
|---|---|
| `workspaces cfg` | one clickable block per rivermonad workspace; the wheel cycles them |
| `layoutName fmt` | layout of the workspace on this output; click = next layout |
| `windowTitle n` | focused window's title, cut to `n` characters |
| `clock fmt` | `formatTime` format, updated on the second |
| `cpu f`, `memory f` | percentages, `f :: Int -> String` |
| `battery f` | `f :: Int -> String -> String` (capacity, status) |
| `command cmd secs` | first line of a shell command, every `secs` seconds |
| `commandReader cmd` | last line printed by a long-running command (markup) |
| `tray defaultTray` | system tray (see below) |
| `text markup` | fixed text |
| `periodic secs io` | any `IO String` (`readCommand` gives a command's first line) |

`WorkspacesConfig` (start from `defaultWorkspaces`) has `wsLabel`,
`focusedStyle`, `activeStyle` (shown on this bar's output), `visibleStyle`
(on another output), `hiddenStyle`, `emptyStyle`, `hideEmpty`, `wsPadding`,
`clickAction` (default `FocusWorkspace . wsName`; `ViewWorkspace` for
greedyView) and `scrollCycles`.

Combinators: `colored "#hex"`, `withStyle`, `padded n`, `markup f`,
`onClick (\c -> ...)`, `spawnOnClick "cmd"`. Text is pango markup; use
`escape` for untrusted strings.

### System tray

`tray defaultTray` shows StatusNotifierItem icons. Left click activates the
application (or opens its menu if it only has one), right click opens its
menu, middle click is the secondary action and the wheel scrolls. waymobar
becomes the `org.kde.StatusNotifierWatcher` on the session bus if nothing
else is, so no extra daemon is needed.

`TrayConfig` fields: `trayIconSize` (0 = bar height − 8), `trayPadding`,
`trayIconThemes` (default: the GTK icon theme from gsettings or
`gtk-3.0/settings.ini`, then Adwaita and hicolor), `trayShowPassive` and
`traySymbolicColor` (colour for monochrome `-symbolic` icons). Run with
`WAYMOBAR_DEBUG=1` to trace what the tray does. Old XEmbed-only tray icons
are not supported (that needs X11).

### Menus

A click handler can open a popup menu under its block:

```haskell
power :: Widget
power = onClick (\c -> clickMenu c
  [ menuItem "Lock" (spawn "swaylock")
  , MenuSeparator
  , Submenu "Power"
      [ menuItem "Suspend" (spawn "systemctl suspend")
      , menuItem "Power off" (spawn "systemctl poweroff") ] ])
  (text "⏻")
```

`MenuItem` also has `itemEnabled`, `itemToggle` (a check mark) and
`itemIcon`. Menus use `bgColor`, `fgColor`, `menuHighlight` and
`menuBorder`. A menu closes when an entry is chosen, when the bar is clicked
or when the pointer leaves it for a moment.

### Writing a widget

A widget is started once and returns what to draw on each output:

```haskell
newtype Widget = Widget { startWidget :: BarEnv -> IO (Output -> IO [Block]) }
```

Fork a thread that updates an `IORef` and calls `redraw env`; the returned
function reads it (it runs on every redraw, so keep it quick). `BarEnv` also
gives `rivermonadState` and `rivermonadDo` (send an `Action`).

## Development

`tools/regen.sh` regenerates `src/Waymobar/Wayland/Protocol.hs` from
`protocol/*.xml`. To test without touching your session, run a headless
river (`WLR_BACKENDS=headless WLR_RENDERER=pixman river -c session.sh` with
a private `XDG_RUNTIME_DIR`) and take screenshots with `grim`.
