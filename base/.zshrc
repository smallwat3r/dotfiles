# smallwat3r's ZSH config entrypoint

# optional profiling (enable by setting DEBUG_ZSH_PERF in env)
if (( ${+DEBUG_ZSH_PERF} )); then
  zmodload zsh/zprof
fi

setopt extended_glob

: "${ZSH_ROOT:=$HOME/.zsh}"

case $OSTYPE in
  darwin*)
    path=(/Applications/Alacritty.app/Contents/MacOS $path)
    : "${TERMINAL:=alacritty}"
    ;;
  linux*)
    [[ -r /etc/os-release ]] && . /etc/os-release
    [[ $ID == fedora ]] && : "${TERMINAL:=foot}"
    if [[ $XDG_CURRENT_DESKTOP == sway ]]; then
      local rundir=/run/user/$UID
      if [[ -z $SWAYSOCK ]]; then
        local sock=($rundir/sway-ipc.*.sock(N[1]))
        [[ -S $sock ]] && export SWAYSOCK=$sock
      fi
      if [[ -z $WAYLAND_DISPLAY ]]; then
        local wl=($rundir/wayland-*~*lock*(N[1]))
        [[ -S $wl ]] && export WAYLAND_DISPLAY=${wl:t}
      fi
    fi
    ;;
esac

: "${TERMINAL:=st}"

: "${TERM:=xterm-256color}"
export TERM TERMINAL

# compile a .zsh file to .zwc if needed
_zsh_compile_if_needed() {
  local src=$1 dst="${1}.zwc"
  [[ -n $src && -r $src ]] || return 1
  if [[ ! -f $dst || $src -nt $dst ]]; then
    # compile silently
    zcompile "$src" 2>/dev/null
  fi
}

load_zsh_dir() {
  local dir=$1 file
  [[ -d $dir && -r $dir ]] || return 0
  for file in "$dir"/*.zsh(N); do
    [[ -r $file ]] || continue
    _zsh_compile_if_needed "$file"
    source "$file"
  done
}

load_zsh_functions() {
  local fn_dir=${1}/functions
  [[ -d $fn_dir ]] || return 0

  fpath=("$fn_dir" $fpath)
  autoload -U "$fn_dir"/*(:tN)
}

load_zsh_config() {
  if [[ -d $ZSH_ROOT ]]; then
    for dir in core tools; do
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
