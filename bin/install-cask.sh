#!/bin/bash
# dotfiles setup script - install casks

if ! brew cask list | grep -ri $1 >/dev/null; then
  name=$(echo $1 | sed -E 's/[-_]+/ /g')
  if ! ls /Applications | grep -ri "$name" >/dev/null; then
    echo "[+] installing $1"
    brew cask install $1
  else
    echo "[.] $1 seems already installed in /Applications"
  fi
else
  echo "[.] $1 already installed via Homebrew"
fi
