__load_plugins() {
  if [[ $(uname) == 'Darwin' ]]; then
    local plugins=(
      '/opt/homebrew/share/zsh-autosuggestions/zsh-autosuggestions.zsh'
      '/opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh'
    )
  else
    local plugins=(
      '/usr/share/zsh/plugins/zsh-autocomplete/zsh-autocomplete.plugin.zsh'
      '/usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh'
      '/usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.plugin.zsh'
    )
  fi
  local plugin
  for plugin ("${plugins[@]}"); do
    if [ -f "${plugin}" ]; then
      source "${plugin}"
    fi
  done
}

__load_plugins
