# Clipboard manager
# Dependencies: clipmenu

if (( $+commands[clipmenu] )); then
  export CM_DIR=/tmp
  export CM_MAX_CLIPS=100
fi
