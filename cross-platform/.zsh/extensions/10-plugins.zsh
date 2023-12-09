__load_plugins() {
  local plugins=(
    # macos
    '/opt/homebrew/share/zsh-autosuggestions/zsh-autosuggestions.zsh'
    '/opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh'
    # linux
    '/usr/share/zsh/plugins/zsh-autocomplete/zsh-autocomplete.plugin.zsh'
    '/usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh'
    '/usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.plugin.zsh'
  )

  local plugin
  for plugin ("${plugins[@]}"); do
    if [ -f "${plugin}" ]; then
      source "${plugin}"
    fi
  done
}

__load_plugins
