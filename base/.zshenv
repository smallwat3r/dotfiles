# ~/.zshenv

# Skip /etc/zsh* system configs
setopt no_global_rcs

: "${XDG_CONFIG_HOME:=$HOME/.config}"
export XDG_CONFIG_HOME

# use unique arrays for paths
typeset -U path cdpath manpath

path=(
  /usr/local/bin
  /usr/local/sbin
  /usr/bin
  /bin
  /sbin
  /usr/sbin
  "$HOME/.local/bin"
  "$HOME/go/bin"
)

cdpath=(
  "$HOME"
)

manpath=(
  /usr/local/share/man
  /usr/share/man
  $manpath
)

# homebrew
/opt/homebrew/bin(/N) && path=(/opt/homebrew/bin $path)
/opt/homebrew/sbin(/N) && path=(/opt/homebrew/sbin $path)
/opt/homebrew/opt/sphinx-doc/bin(/N) && path=(/opt/homebrew/opt/sphinx-doc/bin $path)

# user npm global bin dir
[[ -d "$HOME/.npm-global/bin" ]] && path=("$HOME/.npm-global/bin" $path)

# ssh-agent socket (systemd user service on Linux)
[[ -S "$XDG_RUNTIME_DIR/ssh-agent.socket" ]] \
  && export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"

export PATH  # syncs with $path

export EDITOR="ec"
export LANG="en_US.UTF-8"
export LANGUAGE="en_US.UTF-8"
export CLICOLOR=1

# private environment overrides
[[ -f "$HOME/.zshenv.private" ]] && source "$HOME/.zshenv.private"
