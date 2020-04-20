#!/usr/bin/env bash
# dotfiles setup script - install brew packages

brew ls --versions $1 >/dev/null &&
  printf "[.] $1 already installed.\n" ||
  {
    printf "[+] installing $1\n"
    brew install $1
  }
