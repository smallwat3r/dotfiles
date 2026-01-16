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

# create short `g<alias>` versions of all git aliases.
() {
  command -v git >/dev/null 2>&1 || return

  local line key name
  local git_alias_lines

  git_alias_lines=("${(@f)$(git config --get-regexp '^alias\.' 2>/dev/null)}")

  for line in $git_alias_lines; do
    key=${line%% *}       # "alias.co"
    name=${key#alias.}    # "co"
    alias "g${name}=git ${name}"
  done

  alias g="git"
}

# Use Neovim over Vim
alias \
  vim="nvim" \
  v="nvim"

# Ripgrep
alias rg="rg \
  --max-columns 120 \
  --glob '!git/*' \
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
if [[ "$OSTYPE" =~ ^darwin ]]; then
  alias fonts="open $HOME/Library/Fonts"
  alias tailscale='/Applications/Tailscale.app/Contents/MacOS/Tailscale'
fi

# Global aliases
alias \
    -g G="| grep" \
    -g L="| less" \
    -g NE="2> /dev/null" \
    -g H="| head" \
    -g T="| tail" \
    -g S="| sort"

  if [[ "$OSTYPE" =~ ^darwin ]]; then
    alias -g C="| pbcopy"
  elif command -v wl-copy >/dev/null 2>&1; then
    alias -g C="| wl-copy"
  else
    alias -g C="| xclip -selection clipboard"
fi

# Tracked aliases (named directories)
hash -d \
  d="$HOME/dotfiles" \
  dots="$HOME/dotfiles" \
  c="$HOME/code" \
  dw="$HOME/Downloads" \
  de="$HOME/Desktop" \
  ssh="$HOME/.ssh" \
  zsh="$HOME/.zsh" \
  fonts="$HOME/Library/Fonts" \
  config="$HOME/.config" \
  etc="/etc" \
  opt="/opt"
