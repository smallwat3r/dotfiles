#!/bin/zsh
# File  : env.zsh
# Author: Matthieu Petiteau <mpetiteau.pro@gmail.com>
# Date  : 20.12.2019
#
# ZSH Env
#
export PATH=$PATH:$(go env GOPATH)/bin:$HOME/.cargo/bin
export GOPATH=$(go env GOPATH)
