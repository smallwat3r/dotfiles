#!/bin/zsh
# File  : aliases.zsh
# Author: Matthieu Petiteau <mpetiteau.pro@gmail.com>
# Date  : 20.12.2019
#
# ZSH alias
#

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
