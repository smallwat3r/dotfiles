# ZSH completion configuration

autoload -Uz compinit

() {
  # use a cache dir
  local _zcompdump_base=${XDG_CACHE_HOME:-$HOME/.cache}/zsh
  local _zcompdump=$_zcompdump_base/.zcompdump
  mkdir -p "$_zcompdump_base" >/dev/null 2>&1
  if [[ ! -f $_zcompdump ]]; then
    compinit -d "$_zcompdump"
  else
    # -C: skip re-checking functions, trust dump file
    compinit -C -d "$_zcompdump"
  fi
  _zsh_compile_if_needed "$_zcompdump"
}

setopt COMPLETE_IN_WORD  # complete from both end of a word
setopt AUTO_MENU         # use menu completion after the second consecutive request for completion
setopt AUTO_LIST         # list choices on an ambiguous completion
setopt AUTO_PARAM_SLASH  # add a trailing slash instead of a space in case of a directory
setopt ALWAYS_TO_END     # move cursor to the end after completion

# enable caching for faster completion
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path "${HOME}/.cache/.zcompcache"

# completion matching behavior
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' \
       'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

# interface enhancements
zstyle ':completion:*' menu select
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*' verbose no
zstyle ':completion:*' file-sort modification
zstyle ':completion:*' completer _complete _list _match _approximate
zstyle ':completion:*:corrections' format '%F{green}-- %d (errors: %e)%f'
zstyle ':completion:*:descriptions' format '%F{cyan}-- %d%f'
zstyle ':completion:*:messages' format '%F{blue}-- %d%f'
zstyle ':completion:*:warnings' format '%F{magenta}-- no matches found%f'

# grouping and descriptions
zstyle ':completion:*:matches' group 'yes'
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:options' auto-description '%d'
zstyle ':completion:*' group-name ''

# expand aliases with C-a
zle -C alias-expansion complete-word _generic
bindkey '^a' alias-expansion
zstyle ':completion:alias-expansion:*' completer _expand_alias
