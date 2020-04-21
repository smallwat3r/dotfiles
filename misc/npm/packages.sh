#!/usr/bin/env bash
# Install npm packages

printf 'Installing npm packages...\n'

NPM_PACKAGES=(
  asciicast2gif
  http-server
  prettier
  prettydiff
)
npm install -g ${NPM_PACKAGES[@]} >/dev/null 2>&1
