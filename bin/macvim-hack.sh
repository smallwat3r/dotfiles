#!/bin/bash
# File  : macvim-hack.sh
# Author: Matthieu Petiteau <mpetiteau.pro@gmail.com>
# Date  : 27.12.2019

# Macvim hack
if ! brew ls --versions "macvim" >/dev/null; then
    echo "[+] installing macvim"
    brew unlink vim
    brew install macvim
    brew link vim
else
    echo "[.] macvim already installed"
fi
