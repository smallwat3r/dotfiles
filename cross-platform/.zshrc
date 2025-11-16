# smallwat3r's ZSH config entrypoint

# enable/disable profiling by setting DEBUG_ZSH_PERF in env
#   export DEBUG_ZSH_PERF=1
if (( ${+DEBUG_ZSH_PERF} )); then
  zmodload zsh/zprof
fi

# allow override, otherwise default to ~/.zsh
: "${ZSH_ROOT:=$HOME/.zsh}"

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
