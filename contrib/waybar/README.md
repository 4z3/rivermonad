# Native waybar modules for rivermonad

`waybar-0.15.0-rivermonad.patch` adds three modules to
[waybar](https://github.com/Alexays/Waybar) 0.15.0, modelled after
`niri/workspaces`:

| Module | Shows | Click |
|---|---|---|
| `rivermonad/workspaces` | one button per workspace | focus that workspace |
| `rivermonad/window` | title of the focused window | — |
| `rivermonad/layout` | layout of the workspace on this output | next layout |

They talk to rivermonad's IPC socket (`$RIVERMONAD_SOCKET`, or
`$XDG_RUNTIME_DIR/rivermonad-$WAYLAND_DISPLAY.sock`), always enabled, and
reconnect by themselves when rivermonad restarts (`mod-q`). No configuration
is needed on the rivermonad side.

## Build

```sh
sudo apt install meson ninja-build scdoc libgtkmm-3.0-dev libjsoncpp-dev libspdlog-dev \
  libfmt-dev libgtk-layer-shell-dev libxkbregistry-dev wayland-protocols libdbusmenu-gtk3-dev
curl -L https://github.com/Alexays/Waybar/archive/refs/tags/0.15.0.tar.gz | tar xz
cd Waybar-0.15.0
patch -p1 < /path/to/rivermonad/contrib/waybar/waybar-0.15.0-rivermonad.patch
meson setup build --prefix ~/.local
ninja -C build
ninja -C build install   # optional: ~/.local/bin/waybar
```

## Configuration

```jsonc
"modules-left": ["rivermonad/workspaces", "rivermonad/layout"],
"modules-center": ["rivermonad/window"],
"rivermonad/workspaces": {
  "format": "{name}",        // also {icon} {index} {windows} {layout} {output}
  "hide-empty": true,        // like xmonad's ppHiddenNoWindows = const ""
  "click-action": "focus"    // "focus" = xmonad's view, "view" = greedyView
},
"rivermonad/layout": { "format": "[{}]" },
"rivermonad/window": { "max-length": 60 }
```

```css
#workspaces button.empty   { color: #4c566a; }
#workspaces button.visible { color: #ebcb8b; }          /* on some output */
#workspaces button.active  { border-bottom: 2px solid #88c0d0; } /* on this output */
#workspaces button.focused { background: #88c0d0; color: #2e3440; }
```

Like in xmonad, workspaces are not tied to an output: every bar lists all of
them. See `man waybar-rivermonad-workspaces` for every option.

## The IPC protocol

Newline-delimited JSON, close to niri's. Try it with `rivermonad --ipc`:

```sh
rivermonad --ipc '"EventStream"'                          # stream of {"State":{...}}
rivermonad --ipc '{"Action":{"FocusWorkspace":"3"}}'
rivermonad --ipc '{"Action":"NextLayout"}'
```

Actions: `{"FocusWorkspace":name}`, `{"ViewWorkspace":name}`, `"NextLayout"`,
`{"JumpToLayout":name}`, `"FocusUp"`, `"FocusDown"`, `"Restart"`.
