# External zsh plugins
#
# Loads zsh-autosuggestions and zsh-syntax-highlighting with deferred
# loading for faster shell startup. Plugins are sourced on first prompt.

__load_plugins() {
  emulate -L zsh

  # plugin directory roots to probe per OS
  local -a basedirs
  case $OSTYPE in
    darwin*) basedirs=(/opt/homebrew/share) ;;
    linux*) basedirs=(/usr/share /usr/share/zsh) ;;
    *) basedirs=() ;;
  esac

  # plugin names, used to build paths
  local -a plugins=(
    zsh-autosuggestions
    zsh-syntax-highlighting
  )

  local plugin dir path loaded
  for plugin in $plugins; do
    loaded=0
    for dir in $basedirs; do
      for path in \
        "$dir/$plugin/$plugin.zsh" \
        "$dir/plugins/$plugin/$plugin.plugin.zsh"
      do
        if [[ -f $path ]]; then
          source "$path"
          loaded=1
          break 2   # break out of both inner loops
        fi
      done
    done
    (( loaded )) || print -u2 "Plugin not found: $plugin"
  done
}

__set_zsh_highlight_styles() {
  ZSH_HIGHLIGHT_STYLES[default]=none
  ZSH_HIGHLIGHT_STYLES[unknown-token]=fg=red,bold
  ZSH_HIGHLIGHT_STYLES[reserved-word]=fg=green
  ZSH_HIGHLIGHT_STYLES[alias]=none
  ZSH_HIGHLIGHT_STYLES[builtin]=none
  ZSH_HIGHLIGHT_STYLES[function]=none
  ZSH_HIGHLIGHT_STYLES[command]=none
  ZSH_HIGHLIGHT_STYLES[precommand]=none
  ZSH_HIGHLIGHT_STYLES[commandseparator]=none
  ZSH_HIGHLIGHT_STYLES[hashed-command]=none
  ZSH_HIGHLIGHT_STYLES[path]=none
  ZSH_HIGHLIGHT_STYLES[globbing]=none
  ZSH_HIGHLIGHT_STYLES[history-expansion]=fg=blue
  ZSH_HIGHLIGHT_STYLES[single-hyphen-option]=none
  ZSH_HIGHLIGHT_STYLES[double-hyphen-option]=none
  ZSH_HIGHLIGHT_STYLES[back-quoted-argument]=none
  ZSH_HIGHLIGHT_STYLES[single-quoted-argument]=fg=yellow
  ZSH_HIGHLIGHT_STYLES[double-quoted-argument]=fg=yellow
  ZSH_HIGHLIGHT_STYLES[dollar-double-quoted-argument]=fg=cyan
  ZSH_HIGHLIGHT_STYLES[back-double-quoted-argument]=fg=cyan
  ZSH_HIGHLIGHT_STYLES[assign]=none
  ZSH_HIGHLIGHT_REGEXP+=('^rm .*' fg=90,bold)
  ZSH_HIGHLIGHT_REGEXP+=('\bsudo\b' fg=164,bold)
}

# pre-set config vars before plugins load
ZSH_HIGHLIGHT_HIGHLIGHTERS+=(main brackets regexp)
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=242'

# defer plugin loading until first prompt for faster shell startup
__deferred_load_plugins() {
  __load_plugins
  __set_zsh_highlight_styles
  # reinitialize autosuggestions after deferred load
  (( $+functions[_zsh_autosuggest_start] )) && _zsh_autosuggest_start
  # unhook after first run
  add-zsh-hook -d precmd __deferred_load_plugins
  unfunction __deferred_load_plugins
}

autoload -Uz add-zsh-hook
add-zsh-hook precmd __deferred_load_plugins
