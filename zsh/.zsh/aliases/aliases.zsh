alias c="clear"
alias p="pwd"
alias qq="exit"

# Handy to allow ignoring prompt characters when copying commands from
# documentation. Also acts as an alias to make a command not appear in
# the history when using HISTIGNORESPACE
alias \$=" "
alias %=" "

alias l="ls -pFf"
alias ll="ls -lah"
alias ls.="ls -pF -d .*"
alias ls="ls -pF"
alias sl="ls -pF"
alias lss="ls -la | grep --color=never '->'"
alias mkdir="mkdir -pv"

alias g="git"

alias k="kubectl"
alias kt="kubetail"
alias kgn="kubectl get namespaces"

alias python="python3"
alias pip="pip3"

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

alias v="nvim"
alias vi="nvim"
alias vidiff="nvim -d"
alias vim="nvim"
alias vimdiff="nvim -d"
alias svi="sudo nvim"

alias rg="rg \
  --max-columns 120 \
  --glob '!git/*' \
  --glob '!__pycache__/*' \
  --smart-case \
  --colors 'line:fg:cyan' \
  --colors 'path:fg:cyan' \
  --colors 'match:fg:red' "

alias ctags="/usr/local/bin/ctags"
alias diskspace="df -P -kHl"

alias dots="cd $HOME/dotfiles"
alias fonts="open $HOME/Library/Fonts"

alias sp="tmux splitw -v"
alias tks="tmux kill-session -t"
alias tksa="tmux kill-session -a"
alias tls="tmux list-sessions"
alias vs="tmux splitw -h"
