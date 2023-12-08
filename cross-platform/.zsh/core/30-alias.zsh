alias c="clear"
alias p="pwd"
alias qq="exit"
alias e="$EDITOR"

# Handy to allow ignoring prompt characters when copying commands from
# documentation. Also acts as an alias to make a command not appear in
# the history when using HISTIGNORESPACE.
alias \
  \$=" " \
  %=" "

# List files
alias \
  l="ls -pFf" \
  ll="ls -lah" \
  ls.="ls -pF -d .*" \
  ls="ls -pF" \
  sl="ls -pF" \
  lss="ls -l *(@)"

# Define git aliases from git config.
#
# For all aliases set up in git config, define another alias such as all git
# commands can be called using `g` directly with its alias concatenated.
#
# Examples:
#   `git push` could be called from `gp`.
#   `git checkout` could be called from `gco`.
for al in ${${${(0)"$(git config -z --get-regexp '^alias.')"}%%$'\n'*}\#alias.}; do
  alias g$al="git $al"
done
alias g="git"

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
alias fonts="open $HOME/Library/Fonts"

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
else
  alias -g C="| xclip -selection clipboard"
fi

# Tracked aliases
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
