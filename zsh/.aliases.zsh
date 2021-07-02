# smallwat3r's aliases

# Directory shortcuts
hash -d d="$HOME/dotfiles"
hash -d g="$HOME/Github"
hash -d dw="$HOME/Downloads"
hash -d de="$HOME/Desktop"
hash -d ssh="$HOME/.ssh"
hash -d zsh="$HOME/.zsh"
hash -d tmux="$HOME/.config/tmux"
hash -d nvim="$HOME/.config/nvim"
hash -d doom="$HOME/.doom.d"
hash -d fonts="$HOME/Library/Fonts"
hash -d dots="$HOME/dotfiles"

# Global aliases
alias -g G="| grep"
alias -g L="| less"
alias -g NE="2> /dev/null"

# General
alias c="clear"
alias p="pwd"
alias l="ls -pFf"
alias ll="ls -la"
alias ls.="ls -pF -d .*"
alias ls="ls -pF"
alias sl="ls -pF"
alias lss="ls -la | grep --color=never '->'"
alias mkdir="mkdir -pv"
alias qq="exit"

# Editing stuff
alias e="$EDITOR"
alias edit="$EDITOR"
alias eala="$EDITOR $HOME/.config/alacritty/alacritty.yml"
alias eali="$EDITOR $HOME/.zsh/aliases.zsh"
alias efun="$EDITOR $HOME/.zsh/functions.zsh"
alias egit="$EDITOR $HOME/.config/git/config"
alias ekit="$EDITOR $HOME/.config/kitty/kitty.conf"
alias etmu="$EDITOR $HOME/.config/tmux/tmux.conf"
alias evim="$EDITOR $HOME/.config/nvim/init.vim"
alias ezsh="$EDITOR $HOME/.zsh"
alias svi="sudo nvim"
alias ec="emacsclient"
alias v="nvim"
alias vi="nvim"
alias vidiff="nvim -d"
alias vim="nvim"
alias vimdiff="nvim -d"

# Tmux stuff
alias sp="tmux splitw -v"
alias tks="tmux kill-session -t"
alias tksa="tmux kill-session -a"
alias tls="tmux list-sessions"
alias vs="tmux splitw -h"

# Git stuff
alias g="git"
alias ga="git add"
alias gb="git branch"
alias gc!="git commit -v --amend"
alias gc="git commit -v"
alias gcb="git checkout -b"
alias gcq="git commit -m 'Quick commit'"
alias gd="git diff"
alias gp="git push"
alias gs="git status"

# Kube
alias k="kubectl"
alias kt="kubetail"
alias kgn="kubectl get namespaces"

# gpg
alias gpg-pub-key="gpg --armor --export mpetiteau.pro@gmail.com"
alias gpg-list-keys="gpg --list-secret-keys --keyid-format LONG"

# Misc stuff
alias python="python3"
alias pip="pip3"
alias batt="bluebatt"
alias ctags="/usr/local/bin/ctags"
alias diskspace="df -P -kHl"
alias dots="cd $HOME/dotfiles"
alias fonts="open $HOME/Library/Fonts"
alias sk="sketch"
alias ww="vifm ."