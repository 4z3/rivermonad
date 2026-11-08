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
  recompiles and restarts while keeping each window on its workspace

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
`~/.config/rivermonad/build` script next to the config; it receives the output
path as `$1`. For example, to build against a rivermonad checkout:

```sh
#!/bin/sh
CFG="$PWD"
cd ~/src/rivermonad && exec env -u XDG_CACHE_HOME cabal -v0 exec -- \
  ghc --make "$CFG/rivermonad.hs" -i"$CFG/lib" -threaded -v0 \
  -outputdir "$(dirname "$1")/build" -o "$1"
```

Key bindings take xkbcommon keysyms (`xK_*`, the same values as in X11), using
the unshifted key together with `shiftMask`.

### Differences from xmonad

* Layouts are pure (`pureLayout`/`pureMessage`); there is no `doLayout` in `X`
  yet, and xmonad-contrib does not compile against rivermonad.
* New Wayland windows have no position of their own, so floating windows are
  centred at the size they choose.
* A restart keeps each window's workspace and floating geometry, but not the
  layout state of each workspace.

## Development

```sh
tools/regen.sh      # regenerate Rivermonad/Wayland/Protocol.hs from protocol/*.xml
cabal build
cabal test
```

Protocol files come from river v0.5.0. To try it without leaving your current
session, run river nested inside another Wayland compositor:
`river -c 'rivermonad'` from a terminal.
