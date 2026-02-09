# zsh-syntax-highlighting
#
# Deferred loading for faster shell startup. Plugin is sourced on
# first prompt.

__load_syntax_highlighting() {
  emulate -L zsh

  local -a candidates
  case $OSTYPE in
    darwin*)
      candidates=(
        /opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
      ) ;;
    linux*)
      candidates=(
        /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
        /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.plugin.zsh
      ) ;;
  esac

  local f
  for f in $candidates; do
    if [[ -f $f ]]; then
      source "$f"
      return
    fi
  done
  print -u2 "Plugin not found: zsh-syntax-highlighting"
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

ZSH_HIGHLIGHT_HIGHLIGHTERS+=(main brackets regexp)

__deferred_load_plugins() {
  __load_syntax_highlighting
  __set_zsh_highlight_styles
  add-zsh-hook -d precmd __deferred_load_plugins
  unfunction __deferred_load_plugins
}

autoload -Uz add-zsh-hook
add-zsh-hook precmd __deferred_load_plugins
