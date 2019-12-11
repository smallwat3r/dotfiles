#!/bin/zsh
# File  : zshrc
# Author: Matthieu Petiteau <mpetiteau.pro@gmail.com>
# Date  : 26.10.2019
#
# Zshrc config.
#

export ZSH="/Users/smallwat3r/.oh-my-zsh"

# prompt
local return_code="%(?..%{$fg[red]%}%? ↵%{$reset_color%})"
PROMPT='%{$fg_bold[white]%}$(git_prompt_info)%20<...<%~%<< %{$reset_color%}%% '
RPS1="${return_code}"

# general config
DISABLE_AUTO_TITLE="true"
COMPLETION_WAITING_DOTS="true"
DISABLE_UNTRACKED_FILES_DIRTY="true"

# plugins
plugins=(
  git
  zsh-syntax-highlighting
  zsh-autosuggestions
)

source $ZSH/oh-my-zsh.sh

# paths
export PATH=$PATH:$(go env GOPATH)/bin
export GOPATH=$(go env GOPATH)
export EDITOR=/usr/local/bin/vim

# aliases
alias vi="vim"
alias mvi="mvim"
alias edit="vim"
alias e="vim"
alias viedit="$EDITOR $HOME/dotfiles/vimrc"

alias p="pwd"
alias lg="lazygit"
alias ddg="ddgr"

alias venv="virtualenv -p python3 venv --no-site-package"

alias c="clear"
alias :q="exit"

alias g="git"
alias ga="git add"
alias gs="git status"
alias gc="git commit"
alias gp="git push"

alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."

# loads
if command -v fink>/dev/null; then
  source /sw/bin/init.sh
fi

if command -v tmux>/dev/null; then
  [[ ! $TERM =~ screen ]] && [ -z $TMUX ] && tmux
fi
