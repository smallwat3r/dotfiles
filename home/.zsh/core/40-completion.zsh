# Completion configuration
#
# Sets up zsh completion with caching, case-insensitive matching,
# menu selection, and styled output. Ctrl+A expands aliases.

setopt COMPLETE_IN_WORD  # complete from both end of a word
setopt AUTO_MENU         # use menu after second consecutive tab
setopt AUTO_LIST         # list choices on an ambiguous completion
setopt AUTO_PARAM_SLASH  # trailing slash for directories
setopt ALWAYS_TO_END     # move cursor to end after completion

# enable caching for faster completion
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path "${XDG_CACHE_HOME:-$HOME/.cache}/zsh/.zcompcache"

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

# compinit -C skips the security check on the dump, the dump itself
# is rebuilt only when missing or older than a day (new completions).
autoload -Uz compinit
__zsh_dump_dir=${XDG_CACHE_HOME:-$HOME/.cache}/zsh
mkdir -p "$__zsh_dump_dir" 2>/dev/null
if [[ -n $__zsh_dump_dir/.zcompdump(#qN.mh-24) ]]; then
  compinit -C -d "$__zsh_dump_dir/.zcompdump"
else
  compinit -d "$__zsh_dump_dir/.zcompdump"
fi
_zsh_compile_if_needed "$__zsh_dump_dir/.zcompdump"
unset __zsh_dump_dir

zle -C alias-expansion complete-word _generic
bindkey '^a' alias-expansion
