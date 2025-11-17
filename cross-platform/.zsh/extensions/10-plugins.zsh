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

__load_plugins
