# ~/.zshenv

# Skip /etc/zsh* system configs
setopt no_global_rcs

: "${XDG_CONFIG_HOME:=$HOME/.config}"
export XDG_CONFIG_HOME

# use unique arrays for paths
typeset -U path cdpath

# Add directory to PATH if it exists and isn't already there
# Usage: path_add ~/.local/bin
path_add() {
  [[ -d $1 ]] && (( ${path[(Ie)$1]} == 0 )) && path+=("$1")
}

# Prepend directory to PATH (for overriding system commands)
# Usage: path_prepend ~/.npm-global/bin
path_prepend() {
  [[ -d $1 ]] && (( ${path[(Ie)$1]} == 0 )) && path=("$1" $path)
}

# User bin dirs, only added when missing so a parent shell's PATH keeps
# its order (an activated venv, sshd's environment). The session PATH
# itself comes from environment.d/10-path.conf.
for __dir in "$HOME/go/bin" "$HOME/.cargo/bin" "$HOME/.emacs.d/bin" "$HOME/.local/bin"; do
  path_prepend "$__dir"
done
unset __dir

cdpath=(
  "$HOME"
)

# user npm global bin dir
path_prepend "$HOME/.npm-global/bin"
export PATH  # syncs with $path

export EDITOR="ec"
export LANG="en_US.UTF-8"
export LANGUAGE="en_US.UTF-8"
export CLICOLOR=1

# private environment overrides
[[ -f "$HOME/.zshenv.private" ]] && source "$HOME/.zshenv.private"
