# Manage any external tools / scripts.
# For instance I find autosuggestions and highlighting quite useful.

__load_plugins() {
  if [[ $(uname) == 'Darwin' ]]; then
    local plugins=(
      '/opt/homebrew/share/zsh-autosuggestions/zsh-autosuggestions.zsh'
      '/opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh'
      '/opt/homebrew/share/zsh-history-substring-search/zsh-history-substring-search.zsh'
    )
  elif [[ $(grep 'DEFAULT_HOSTNAME' /etc/os-release) == *fedora* ]]; then
    local plugins=(
      '/usr/share/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh'
      '/usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.plugin.zsh'
      # TODO: no dnf package seems to exists for this plugin...
      # '/usr/share/zsh-history-substring-search/zsh-history-substring-search.plugin.zsh'
    )
  else
    local plugins=(
      '/usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh'
      '/usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.plugin.zsh'
      '/usr/share/zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.plugin.zsh'
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
