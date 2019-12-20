#!/bin/zsh
# File  : zshrc
# Author: Matthieu Petiteau <mpetiteau.pro@gmail.com>
# Date  : 26.10.2019
#
# ZSH config.
#

export ZSH="$HOME/.oh-my-zsh"
export ZSH_CONF="$HOME/.zsh"

source $ZSH_CONF/antigen.zsh
antigen use oh-my-zsh

# bundles
antigen bundle git
antigen bundle pip
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-autosuggestions
antigen bundle zsh-users/zsh-completions

antigen apply

export CLICOLOR=1
export EDITOR=/usr/local/bin/vim

DISABLE_AUTO_TITLE="true"
COMPLETION_WAITING_DOTS="true"
DISABLE_UNTRACKED_FILES_DIRTY="true"

source $ZSH/oh-my-zsh.sh

source $ZSH_CONF/prompt.zsh
source $ZSH_CONF/env.zsh
source $ZSH_CONF/aliases.zsh

[ -f "`which tmux`" ] && [ -z $TMUX ] && tmux
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
