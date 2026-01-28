alias c="clear"
alias p="pwd"
alias qq="exit"
alias e="$EDITOR"

# Handy to allow ignoring prompt characters when copying commands from
# documentation. Also acts as an alias to make a command not appear in
# the history when using HIST_IGNORE_SPACE (HIST_IGNORE_SPACE in zsh).
alias \
  \$=" " \
  %=" "

# List files
alias \
  l="ls -pFf --color" \
  ll="ls -lah --color" \
  ls.="ls -pF -d .* --color" \
  ls="ls -pF --color" \
  sl="ls -pF --color" \
  lss="ls -l *(@)"

# Use Neovim over Vim
alias \
  vim="nvim" \
  v="nvim"

# Ripgrep
alias rg="rg \
  --max-columns 120 \
  --glob '!.git/*' \
  --glob '!__pycache__/*' \
  --smart-case \
  --colors 'line:fg:cyan' \
  --colors 'path:fg:cyan' \
  --colors 'match:fg:red' "

# Misc
alias mkdir="mkdir -pv"
alias diskspace="df -P -kHl"
alias dots="cd $HOME/dotfiles"

# macOS-only helpers
is_macos && alias fonts="open $HOME/Library/Fonts"

# Global aliases
alias \
    -g G="| grep" \
    -g L="| less" \
    -g NE="2> /dev/null" \
    -g H="| head" \
    -g T="| tail" \
    -g S="| sort"

alias -g C="| $(_clip_cmd)"

# Tracked aliases (named directories)
hash -d \
  d="$HOME/dotfiles" \
  dots="$HOME/dotfiles" \
  c="$HOME/code" \
  dw="$HOME/Downloads" \
  de="$HOME/Desktop" \
  ssh="$HOME/.ssh" \
  zsh="$HOME/.zsh" \
  config="$HOME/.config" \
  etc="/etc" \
  opt="/opt"

is_macos && hash -d fonts="$HOME/Library/Fonts"
