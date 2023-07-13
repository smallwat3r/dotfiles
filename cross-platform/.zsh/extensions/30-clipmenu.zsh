# Clipboard manager
# Dependencies: clipmenu

if [ -f /usr/bin/clipmenu ]; then
  export CM_DIR=/tmp
  export CM_MAX_CLIPS=100
fi
