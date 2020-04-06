#!/usr/bin/env bash
# Install npm packages

NPM_PACKAGES=(
  asciicast2gif
  http-server
  prettier
  prettydiff
)
npm install -g ${NPM_PACKAGES[@]}
