#!/bin/bash
# File  : install-brew.sh
# Author: Matthieu Petiteau <mpetiteau.pro@gmail.com>
# Date  : 27.12.2019

if ! brew ls --versions $1 >/dev/null; then
    echo "[+] installing $1"
    brew install $1
else
    echo "[.] $1 already installed"
fi
