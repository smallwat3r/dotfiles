# smallwat3r's ZSH config entrypoint

# optional profiling (enable by setting DEBUG_ZSH_PERF in env)
if (( ${+DEBUG_ZSH_PERF} )); then
  zmodload zsh/zprof
fi

setopt extended_glob

: "${ZSH_ROOT:=$HOME/.zsh}"

case $(uname -s) in
  Darwin)
    path=(/Applications/Alacritty.app/Contents/MacOS $path)
    : "${TERMINAL:=alacritty}"
    ;;
  Linux)
    if [[ -r /etc/os-release ]]; then
      . /etc/os-release
      case $ID in
        fedora)  : "${TERMINAL:=foot}" ;;
        *)       : "${TERMINAL:=st}"   ;;
      esac
    else
      : "${TERMINAL:=st}"
    fi
    ;;
  *) : "${TERMINAL:=st}" ;;
esac

: "${TERM:=xterm-256color}"
export TERM TERMINAL

load_zsh_dir() {
  local dir=$1

  if [[ -d $dir ]]; then
    for file in $dir/*.zsh(N); do
      source "$file"
    done
  else
    printf 'Could not find configs in %s\n' "$dir" >&2
  fi
}

load_zsh_functions() {
  local root=$1 fn_dir=$root/functions

  [[ -d $fn_dir ]] || return 0

  fpath=("$fn_dir" $fpath)
  autoload -U "$fn_dir"/*(:tN)
}

load_zsh_config() {
  if [[ -d $ZSH_ROOT ]]; then
    for dir in core extensions; do
      load_zsh_dir "$ZSH_ROOT/$dir"
    done
    load_zsh_functions "$ZSH_ROOT"
  else
    printf 'ZSH_ROOT not found at %s\n' "$ZSH_ROOT" >&2
  fi

  # private configs
  [[ -f $HOME/.zshrc.private ]] && source "$HOME/.zshrc.private"
}

load_zsh_config

if (( ${+DEBUG_ZSH_PERF} )); then
  zprof
fi
