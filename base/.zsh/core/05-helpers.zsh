# Shared helper functions for zsh configuration
#
# Common utilities used throughout the zsh config. Loaded early (05-)
# so all other config files can use these helpers.

# OS detection
[[ "$OSTYPE" =~ ^darwin ]] && __IS_MACOS=1 || __IS_MACOS=0
[[ "$OSTYPE" =~ ^linux ]] && __IS_LINUX=1 || __IS_LINUX=0

is_macos() { (( __IS_MACOS )); }
is_linux() { (( __IS_LINUX )); }

# Command existence check
# Usage: has git && git status
# Usage: has git fzf jq && echo "all installed"
has() {
  local cmd
  for cmd in "$@"; do
    (( $+commands[$cmd] )) || return 1
  done
}

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

# Clipboard operations
# Usage: echo "text" | clip
clip() {
  if is_macos; then
    pbcopy
  elif has wl-copy; then
    wl-copy
  elif has xclip; then
    xclip -selection clipboard
  else
    cat
    echo 'No clipboard tool found, printed to stdout.' >&2
    return 1
  fi
}

# Returns the clipboard command for use in aliases
# Usage: alias -g C="| $(_clip_cmd)"
_clip_cmd() {
  if is_macos; then
    echo "pbcopy"
  elif has wl-copy; then
    echo "wl-copy"
  elif has xclip; then
    echo "xclip -selection clipboard"
  else
    echo "cat"
  fi
}
