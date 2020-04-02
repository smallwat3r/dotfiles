#!/bin/bash
# dotfiles setup script - install brew packages

if ! brew ls --versions $1 >/dev/null; then
  echo "[+] installing $1"
  brew install $1
else
  echo "[.] $1 already installed"
fi
