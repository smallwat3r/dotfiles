# Zsh completion
# ~~~~~~~~~~~~~~

autoload -U compinit

zstyle ':completion:*'      list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*'      menu select
zstyle ':completion:*'      matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*'      accept-exact '*(N)'
zstyle ':completion:*'      use-cache on
zstyle ':completion:*'      cache-path ~/.cache
zstyle ':completion:*:rm:*' ignore-line-yes

zmodload zsh/complist

compinit
