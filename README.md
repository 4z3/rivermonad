# rivermonad

An xmonad-style tiling window manager for Wayland. It is to xmonad what sway is
to i3: the same model and the same kind of Haskell configuration file, on a
Wayland compositor.

rivermonad is **not** a compositor. It runs as the window manager process of
[river](https://codeberg.org/river/river) ≥ 0.4, which splits the compositor
(rendering, input, protocols, Xwayland) from the window manager, connected by
the stable `river-window-management-v1` protocol. Because of this split:

* if rivermonad crashes or restarts, your windows survive;
* rivermonad contains no wlroots, C or FFI code: the Wayland wire protocol is
  implemented in pure Haskell (`Rivermonad.Wayland.Wire`) and the bindings are
  generated from river's XML files (`tools/gen-protocol.py`).

## What works

* `StackSet` with an API compatible with `XMonad.StackSet`: workspaces,
  multiple monitors, floating layer
* layouts `Tall`, `Mirror`, `Full` and `|||`, plus the `Shrink`/`Expand`,
  `IncMasterN`, `NextLayout` and `JumpToLayout` messages
* `ManageHook` with `appId`/`className`, `title`, `isDialog`, `doShift`,
  `doFloat`, `doCenterFloat`, `doFullscreen` and friends
* xmonad's default key bindings (with Super as the modifier), mouse move and
  resize of floating windows, focus follows mouse
* compositor-drawn borders, client fullscreen requests and `mod-f`
* layer-shell (waybar, fuzzel, …), with the space taken by bars excluded
* `~/.config/rivermonad/rivermonad.hs` recompiled with ghc; `mod-q`
  recompiles and restarts while keeping each window's workspace, stack
  position and focus, every workspace's layout state and persistent
  extensible state
* an IPC socket (newline-delimited JSON, like niri's): a state event stream
  and actions, usable from scripts with `rivermonad --ipc`
* xmonad's extension points: layouts in `X` (`doLayout`, `handleMessage`),
  `LayoutModifier`, `ExtensibleState`, `handleEventHook`

## Contrib modules

Ports of the most used xmonad-contrib modules ship in the same package, with
the `XMonad.` prefix replaced by `Rivermonad.`:

| Module | What it does |
|---|---|
| `Rivermonad.Util.EZConfig` | `additionalKeysP` with `"M-S-<Return>"`-style strings |
| `Rivermonad.Util.SpawnOnce` | `spawnOnce`: start programs once per session, not per restart |
| `Rivermonad.Util.NamedScratchpad` | toggleable scratchpad windows |
| `Rivermonad.Util.ExtensibleState` | typed state for your own modules (`XS.get`/`XS.put`) |
| `Rivermonad.Actions.CycleWS` | `nextWS`, `prevWS`, `toggleWS`, `moveTo Next EmptyWS`, screens |
| `Rivermonad.Hooks.ManageHelpers` | `composeOne`, `-?>`, `doRectFloat`, `doFullFloat`, `transience` |
| `Rivermonad.Hooks.InsertPosition` | `insertPosition Below Newer` |
| `Rivermonad.Hooks.StatusBar` | status line for waybar (see below) |
| `Rivermonad.Layout.LayoutModifier` | write your own layout modifiers |
| `Rivermonad.Layout.NoBorders` | `noBorders`, `smartBorders` |
| `Rivermonad.Layout.Spacing` | `spacing`, `spacingWithEdge`, `smartSpacing`, `incSpacing` |
| `Rivermonad.Layout.ToggleLayouts` | `toggleLayouts (noBorders Full) myLayouts` |
| `Rivermonad.Layout.PerWorkspace` | `onWorkspace "web" Full myLayouts` |
| `Rivermonad.Layout.ThreeColumns` | `ThreeCol`, `ThreeColMid` |
| `Rivermonad.Layout.Grid` | `Grid` |
| `Rivermonad.Layout.ResizableTile` | `ResizableTall` with `MirrorShrink`/`MirrorExpand` |

Not ported: modules that draw their own UI in X11 (`Prompt`, `GridSelect`,
`Tabbed`, decorations); use fuzzel & co. instead of prompts.

### Status bar

**waymobar** (`waymobar/`) is a native bar written and configured in
Haskell, with rivermonad widgets built in. See
[waymobar/README.md](waymobar/README.md).

With waybar instead:

**Native modules:** `contrib/waybar/` has a patch for waybar 0.15.0 adding
`rivermonad/workspaces` (clickable), `rivermonad/window` and
`rivermonad/layout`, which read rivermonad's IPC socket. See
[contrib/waybar/README.md](contrib/waybar/README.md).

**Unpatched waybar:** `withStatusBar def myConfig` makes rivermonad publish
a status line; `rivermonad --status` prints it as JSON lines, which a waybar
`custom` module reads:

```jsonc
"modules-left": ["custom/rivermonad"],
"custom/rivermonad": {
  "exec": "rivermonad --status",
  "return-type": "json",
  "escape": false
}
```

Customise it with a `PP` like xmonad's (`ppCurrent`, `ppHidden`, `ppTitle`,
`ppOrder`, …); the text uses Pango markup (`pangoColor "#88c0d0"`).

## Installation

```sh
sudo apt install ghc cabal-install    # or ghcup
cabal update
cabal install --lib rivermonad        # so ghc can compile your config
cabal install exe:rivermonad
```

River ≥ 0.4 must be built separately (see its README; it needs zig and
wlroots). Then start the session with:

```sh
river -c rivermonad
```

## Configuration

Copy `examples/rivermonad.hs` to `~/.config/rivermonad/rivermonad.hs`. Moving
over from xmonad mostly means:

| xmonad                          | rivermonad                          |
|---------------------------------|-------------------------------------|
| `import XMonad`                 | `import Rivermonad`                 |
| `import qualified XMonad.StackSet as W` | `import qualified Rivermonad.StackSet as W` |
| `main = xmonad def {...}`       | `main = rivermonad def {...}`       |
| `layoutHook = a ||| b`          | `layoutHook = Layout (a ||| b)`     |
| `className =? "Firefox"`        | `appId =? "firefox"` (`className` is an alias) |
| `dmenu_run`                     | `fuzzel`, `bemenu-run`, `wmenu-run` |

If you'd rather not install the library globally, put an executable
`~/.config/rivermonad/build` script next to the config; rivermonad runs it
with the output binary as `$1`. For example, to build against a rivermonad
checkout in `~/src/rivermonad` (it also works when run by hand):

```sh
#!/bin/sh
set -e
CFG="$(cd "$(dirname "$0")" && pwd)"
CACHE="${XDG_CACHE_HOME:-$HOME/.cache}/rivermonad"
OUT="${1:-$CACHE/rivermonad-$(uname -m)-linux}"
mkdir -p "$(dirname "$OUT")"
cd ~/src/rivermonad
exec env -u XDG_CACHE_HOME -u XDG_CONFIG_HOME cabal -v0 exec -- \
  ghc --make -package rivermonad "$CFG/rivermonad.hs" -i"$CFG/lib" -threaded -v0 \
  -outputdir "$(dirname "$OUT")/build" -o "$OUT"
```

Key bindings take xkbcommon keysyms (`xK_*`, the same values as in X11), using
the unshifted key together with `shiftMask`.

### Differences from xmonad

* xmonad-contrib itself does not compile against rivermonad (it depends on
  X11); the modules above are ports.
* `layoutHook` takes a `Layout`: write `Layout (a ||| b)`.
* New Wayland windows have no position of their own, so floating windows are
  centred at the size they choose.
* Restarts identify windows by river's stable window identifier.

## Development

```sh
tools/regen.sh      # regenerate Rivermonad/Wayland/Protocol.hs from protocol/*.xml
cabal build
cabal test
```

Protocol files come from river v0.5.0. To try it without leaving your current
session, run river nested inside another Wayland compositor:
`river -c 'rivermonad'` from a terminal.
