# river + rivermonad session

Files to start rivermonad as a full session, from a display manager (GDM,
SDDM...) or from a TTY.

| File | Installed to | What it does |
|---|---|---|
| `river-session` | `~/.local/bin/` | sets up the environment and starts river |
| `init` | `~/.config/river/init` | started by river: kanshi, then rivermonad (restarted if it crashes) |
| `rivermonad.desktop` | `~/.local/share/wayland-sessions/` and `/usr/share/wayland-sessions/` | the entry the login screen lists |

## Install

```sh
contrib/session/install.sh            # for your user
contrib/session/install.sh --system   # also list it in GDM (uses sudo)
```

An existing `~/.config/river/init` is never overwritten. Then pick
"river + rivermonad" on the login screen (GDM: the gear button, before
typing the password), or run `river-session` from a TTY.

Everything else (bar, notifications...) is started from
`~/.config/rivermonad/rivermonad.hs` with `spawnOnce`.

## Things this takes care of

* **The session doesn't read `~/.bashrc`.** `river-session` and `init` put
  `~/.local/bin` first in `PATH`, so a river, rivermonad or patched waybar
  installed there wins over system packages.
* **GDM hides sessions it cannot check.** The login screen runs as its own
  user, which usually cannot read your home directory. A `TryExec=` pointing
  into `~` therefore makes the session disappear from the list, so the
  session file has no `TryExec`. `Exec` runs as you, after login, and
  needs an absolute path (display managers don't expand `~`); `install.sh`
  writes it.
* **Virtual machines (QEMU/UTM, virtio-gpu).** wlroots draws the hardware
  cursor upside down there, so the visible tip is not where clicks land
  (clicks on a bar miss it). `river-session` detects virtio-gpu and asks
  for a software cursor (`WLR_NO_HARDWARE_CURSORS=1`), which must be set
  before river starts.
* **Portals.** `init` exports `XDG_CURRENT_DESKTOP=river` and passes it
  to D-Bus/systemd, so screen sharing and file pickers find the session.
* **Monitors.** Resolution and scale are not the window manager's job on
  river: `init` starts [kanshi](https://sr.ht/~emersion/kanshi/) when it is
  installed, configured in `~/.config/kanshi/config`:

  ```
  profile {
      output Virtual-1 scale 2
  }
  ```

  Avoid a fixed `mode` on virtual machines: their resolution follows the
  VM window, and kanshi rejects the whole profile if the mode is missing.
  To reload: `pkill -x kanshi; setsid -f kanshi`.

## Graphics in virtual machines

On UTM (Apple silicon) the guest GPU is `virgl` translated to Metal, which
breaks some GPU text rendering: Firefox may show no text at all. Set
`gfx.webrender.software` to `true` in `about:config` (or in the profile's
`user.js`) so Firefox rasterizes on the CPU.
