# smallwat3r's ZSH config entrypoint

# enable/disable profiling by setting DEBUG_ZSH_PERF in env
if (( ${+DEBUG_ZSH_PERF} )); then
  zmodload zsh/zprof
fi

: "${ZSH_ROOT:=$HOME/.zsh}"

if [[ $(uname) == 'Darwin' ]]; then
  path=(/Applications/Alacritty.app/Contents/MacOS $path)
  export TERMINAL='alacritty'
elif [[ -f /etc/os-release ]] && grep -qi '^ID=fedora' /etc/os-release; then
  export TERMINAL='foot'
else
  export TERMINAL='st'
fi

: "${TERM:=xterm-256color}"
export TERM

load_zsh_dir() {
  local dir=$1
  if [[ -d "$dir" ]]; then
    # (N) => null_glob, so empty dirs are fine
    for file in "$dir"/*.zsh(N); do
      source "$file"
    done
  else
    printf 'Could not find configs in %s\n' "$dir"
  fi
}

load_zsh_functions() {
  local root=$1
  local fn_dir="$root/functions"

  if [[ -d "$fn_dir" ]]; then
    fpath=("$fn_dir" $fpath)
    # (:tN) => take basename, null_glob
    autoload -U "$fn_dir"/*(:tN)
  fi
}

load_zsh_config() {
  if [[ -d "$ZSH_ROOT" ]]; then
    for dir in core extensions; do
      load_zsh_dir "$ZSH_ROOT/$dir"
    done
    load_zsh_functions "$ZSH_ROOT"
  else
    printf 'ZSH_ROOT not found at %s\n' "$ZSH_ROOT"
  fi

  # load private configs
  if [[ -f "$HOME/.zshrc.private" ]]; then
    source "$HOME/.zshrc.private"
  fi
}

load_zsh_config

if (( ${+DEBUG_ZSH_PERF} )); then
  zprof
fi
