# Completion configuration
#
# Sets up zsh completion with caching, case-insensitive matching,
# menu selection, and styled output. Ctrl+A expands aliases.
# compinit is deferred to first prompt to keep startup fast.

autoload -Uz compinit

# zstyles can be set before compinit runs
setopt COMPLETE_IN_WORD  # complete from both end of a word
setopt AUTO_MENU         # use menu after second consecutive tab
setopt AUTO_LIST         # list choices on an ambiguous completion
setopt AUTO_PARAM_SLASH  # trailing slash for directories
setopt ALWAYS_TO_END     # move cursor to end after completion

# enable caching for faster completion
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path "${HOME}/.cache/.zcompcache"

# completion matching behavior
zstyle ':completion:*' matcher-list '' \
       'm:{a-zA-Z}={A-Za-z}' \
       'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

# interface enhancements
zstyle ':completion:*' menu select
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*' verbose no
zstyle ':completion:*' file-sort modification
zstyle ':completion:*' completer \
       _complete _list _match _approximate
zstyle ':completion:*:corrections' format \
       '%F{green}-- %d (errors: %e)%f'
zstyle ':completion:*:descriptions' format \
       '%F{cyan}-- %d%f'
zstyle ':completion:*:messages' format \
       '%F{blue}-- %d%f'
zstyle ':completion:*:warnings' format \
       '%F{magenta}-- no matches found%f'

# grouping and descriptions
zstyle ':completion:*:matches' group 'yes'
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:options' auto-description '%d'
zstyle ':completion:*' group-name ''

# expand aliases with C-a
zstyle ':completion:alias-expansion:*' completer _expand_alias

# Defer compinit to first prompt so it doesn't block startup.
__deferred_compinit() {
  local dump_dir=${XDG_CACHE_HOME:-$HOME/.cache}/zsh
  local dump=$dump_dir/.zcompdump
  mkdir -p "$dump_dir" 2>/dev/null

  if [[ ! -f $dump ]]; then
    compinit -d "$dump"
  else
    compinit -C -d "$dump"
  fi
  _zsh_compile_if_needed "$dump"

  # wire up alias-expansion now that compinit has run
  zle -C alias-expansion complete-word _generic
  bindkey '^a' alias-expansion

  add-zsh-hook -d precmd __deferred_compinit
  unfunction __deferred_compinit
}

autoload -Uz add-zsh-hook
add-zsh-hook precmd __deferred_compinit
