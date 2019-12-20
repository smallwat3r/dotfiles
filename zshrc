#!/bin/zsh
# File  : zshrc
# Author: Matthieu Petiteau <mpetiteau.pro@gmail.com>
# Date  : 26.10.2019
#
# Zshrc config.
#
# You might need to download antigen if you don't have it already
# curl -L git.io/antigen > ~/.oh-my-zsh/antigen.zsh
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

# prompt
local return_code="%(?..%{$fg[red]%}%? ↵%{$reset_color%})"
PROMPT='%{$fg_bold[white]%}$(git_prompt_info)%20<...<%~%<< %{$reset_color%}%% '
RPS1="${return_code}"

# general config
DISABLE_AUTO_TITLE="true"
COMPLETION_WAITING_DOTS="true"
DISABLE_UNTRACKED_FILES_DIRTY="true"

source $ZSH/oh-my-zsh.sh

# paths
export PATH=$PATH:$(go env GOPATH)/bin
export GOPATH=$(go env GOPATH)
export EDITOR=/usr/local/bin/vim

# aliases
alias vi="vim"
alias svi="sudo vi"
alias edit="vim"
alias e="vim"
alias mvi="mvim"

alias dots="cd $HOME/dotfiles"
alias vie="$EDITOR $HOME/dotfiles/vimrc"
alias zshe="$EDITOR $HOME/dotfiles/zshrc"

alias python="python3"
alias pip="pip3"

alias lg="lazygit"
alias ddg="ddgr"

alias venv="virtualenv -p python3 venv --no-site-package"

alias c="clear"
alias :q="exit"
alias :x="exit"

alias tks='tmux kill-session -t'
alias tksa='tmux kill-session -a'
alias tls='tmux list-sessions'

alias rm='rm -i'

alias p="pwd"
alias ls.="ls -d .*"
alias hs='history | grep'

alias myip="curl http://ipecho.net/plain; echo"

alias g="git"
alias ga="git add "
alias gs="git status "
alias gc="git commit -m "
alias gp="git push "

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

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
