#!/bin/sh
# Install the river + rivermonad session for the current user:
#   river-session -> ~/.local/bin/
#   init          -> ~/.config/river/init   (kept if it already exists)
#   session file  -> ~/.local/share/wayland-sessions/rivermonad.desktop
# With --system the session file is also copied to
# /usr/share/wayland-sessions/ (needs sudo) so GDM and others list it.
set -e
here="$(cd "$(dirname "$0")" && pwd)"
bindir="$HOME/.local/bin"
config="${XDG_CONFIG_HOME:-$HOME/.config}/river"
sessions="${XDG_DATA_HOME:-$HOME/.local/share}/wayland-sessions"

mkdir -p "$bindir" "$config" "$sessions"
install -m 755 "$here/river-session" "$bindir/river-session"
echo "installed $bindir/river-session"

if [ -e "$config/init" ]; then
  echo "kept existing $config/init (example: $here/init)"
else
  install -m 755 "$here/init" "$config/init"
  echo "installed $config/init"
fi

# Display managers don't expand ~ or $HOME: write the absolute path.
sed "s|@BINDIR@|$bindir|" "$here/rivermonad.desktop" > "$sessions/rivermonad.desktop"
echo "installed $sessions/rivermonad.desktop"

if [ "$1" = "--system" ]; then
  sudo install -m 644 "$sessions/rivermonad.desktop" /usr/share/wayland-sessions/rivermonad.desktop
  echo "installed /usr/share/wayland-sessions/rivermonad.desktop"
else
  echo "GDM only reads system sessions; to list it there run: $0 --system"
fi
