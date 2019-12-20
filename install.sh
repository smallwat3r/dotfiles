#!/bin/bash
# File  : install.sh
# Author: Matthieu Petiteau <mpetiteau.pro@gmail.com>
# Date  : 20.12.2019

which -s brew
if [[ $? != 0 ]] ; then
    # Install Homebrew
    ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
else
    brew update
fi
