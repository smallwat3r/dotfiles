#!/usr/bin/env bash
set -e

if [[ "$OSTYPE" =~ ^darwin ]]; then
  pbcopy
else
  xclip -selection clipboard
fi
