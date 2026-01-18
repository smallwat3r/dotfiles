#!/usr/bin/env bash
set -e

copy() {
  if [[ "$OSTYPE" =~ ^darwin ]]; then
    pbcopy
  elif [[ -n "$WAYLAND_DISPLAY" ]] && command -v wl-copy >/dev/null 2>&1; then
    wl-copy
  elif command -v xclip >/dev/null 2>&1; then
    xclip -selection clipboard
  elif command -v xsel >/dev/null 2>&1; then
    xsel --clipboard --input
  else
    echo "No clipboard tool found" >&2
    return 1
  fi
}

copy
