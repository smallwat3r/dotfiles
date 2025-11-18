# Manage any external tools / scripts, such as autosuggestions and syntax highlighting.

__load_plugins() {
  local plugins plugin

  case "$(uname)" in
    Darwin)
      plugins=(
        '/opt/homebrew/share/zsh-autosuggestions/zsh-autosuggestions.zsh'
        '/opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh'
        '/opt/homebrew/share/zsh-history-substring-search/zsh-history-substring-search.zsh'
      )
      ;;
    Linux)
      # Fedora detection
      if [[ -f /etc/os-release ]] && grep -qi '^id=fedora' /etc/os-release; then
        plugins=(
          '/usr/share/zsh-autosuggestions/zsh-autosuggestions.zsh'
          '/usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh'
          '/usr/share/zsh-history-substring-search/zsh-history-substring-search.zsh'
        )
      else
        # Generic Linux distros
        plugins=(
          '/usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh'
          '/usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.plugin.zsh'
          '/usr/share/zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.plugin.zsh'
        )
      fi
      ;;
    *)
      plugins=() # Unknown system
      ;;
  esac

  for plugin in "${plugins[@]}"; do
    if [[ -f $plugin ]]; then
      source "$plugin"
    else
      echo "Plugin not found: $plugin" >&2
    fi
  done
}

ZSH_HIGHLIGHT_HIGHLIGHTERS+=(main brackets regexp)

__load_plugins

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
