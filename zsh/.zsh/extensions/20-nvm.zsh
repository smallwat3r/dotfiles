export NVM_DIR="${HOME}/.nvm"

# I find nvm quite bulky to load on startup, so this provides a way to lazy load it.
nvm() {
  if ! command -v nvm --help >/dev/null; then
    if [ -s "/usr/local/opt/nvm/nvm.sh" ]; then
      source "/usr/local/opt/nvm/nvm.sh"
    fi
    if [ -s "/usr/local/opt/nvm/etc/bash_completion.d/nvm" ]; then
      source "/usr/local/opt/nvm/etc/bash_completion.d/nvm"
    fi
  fi

  nvm "${@}"
}
