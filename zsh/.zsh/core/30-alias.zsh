alias c="clear"
alias p="pwd"
alias qq="exit"
alias e="$EDITOR"

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
alias diskspace="df -P -kHl"

alias g="git"
alias python="python3"
alias pip="pip3"

alias rg="rg \
  --max-columns 120 \
  --glob '!git/*' \
  --glob '!__pycache__/*' \
  --smart-case \
  --colors 'line:fg:cyan' \
  --colors 'path:fg:cyan' \
  --colors 'match:fg:red' "

alias dots="cd $HOME/dotfiles"
alias fonts="open $HOME/Library/Fonts"

#global aliases
alias -g G="| grep"
alias -g L="| less"
alias -g NE="2> /dev/null"
alias -g C="| pbcopy"
alias -g H="| head"
alias -g T="| tail"
alias -g S="| sort"

# tracked aliases
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
hash -d config="$HOME/.config"
hash -d etc="/etc"
hash -d opt="/opt"
