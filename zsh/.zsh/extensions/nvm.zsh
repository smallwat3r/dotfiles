export NVM_DIR="$HOME/.nvm"

# I find nvm quite bulky in my zsh config, so this provides a way to lazy load it
nvm() {
  if ! command -v foo >/dev/null; then
    [ -s "/usr/local/opt/nvm/nvm.sh" ] && source "/usr/local/opt/nvm/nvm.sh"
    [ -s "/usr/local/opt/nvm/etc/bash_completion.d/nvm" ] && source "/usr/local/opt/nvm/etc/bash_completion.d/nvm"
  fi

  nvm "$@"
}
