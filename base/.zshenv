# ~/.zshenv

# Skip /etc/zsh* system configs
setopt no_global_rcs

: "${XDG_CONFIG_HOME:=$HOME/.config}"
export XDG_CONFIG_HOME

# use unique arrays for paths
typeset -U path cdpath manpath

# Add directory to PATH if it exists and isn't already there
# Usage: path_add ~/.local/bin
path_add() {
  [[ -d $1 ]] && (( ${path[(Ie)$1]} == 0 )) && path+=("$1")
}

# Prepend directory to PATH (for overriding system commands)
# Usage: path_prepend /usr/local/opt/grep/libexec/gnubin
path_prepend() {
  [[ -d $1 ]] && (( ${path[(Ie)$1]} == 0 )) && path=("$1" $path)
}

path=(
  "$HOME/.local/bin"
  "$HOME/.emacs.d/bin"
  "$HOME/.cargo/bin"
  "$HOME/go/bin"
  /usr/local/bin
  /usr/local/sbin
  /usr/bin
  /bin
  /sbin
  /usr/sbin
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
path_prepend /opt/homebrew/bin
path_prepend /opt/homebrew/sbin
path_prepend /opt/homebrew/opt/sphinx-doc/bin

# user npm global bin dir
path_prepend "$HOME/.npm-global/bin"

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
