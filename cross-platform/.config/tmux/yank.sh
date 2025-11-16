#!/usr/bin/env bash
set -e

if [[ "$OSTYPE" =~ ^darwin ]]; then
  pbcopy
elif command -v wl-copy >/dev/null 2>&1; then
  wl-copy
else
  xclip -selection clipboard
fi
