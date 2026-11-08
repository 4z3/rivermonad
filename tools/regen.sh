#!/bin/sh
# Regenerate the Haskell protocol bindings from protocol/*.xml.
set -e
cd "$(dirname "$0")/.."
python3 tools/gen-protocol.py \
  protocol/wayland-core.xml \
  protocol/river-window-management-v1.xml \
  protocol/river-xkb-bindings-v1.xml \
  protocol/river-layer-shell-v1.xml \
  > src/Rivermonad/Wayland/Protocol.hs
