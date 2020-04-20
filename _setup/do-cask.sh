#!/usr/bin/env bash
# dotfiles setup script - install casks

brew ls --versions $1 >/dev/null &&
  printf "[.] cask $1 already installed via Homebrew\n" ||
  {
    _name=$(
      echo $1 |
        sed -E 's/[-_]+/ /g'
    )
    ls /Applications | grep -ri "$_name" >/dev/null &&
      printf "[.] $1 seems already installed in /Applications\n" ||
      {
        printf "[+] installing $1\n"
        brew cask install $1
      }
  }
