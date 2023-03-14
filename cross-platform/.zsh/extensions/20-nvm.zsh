export NVM_DIR="${HOME}/.nvm"

# nvm is quite bulky to load on startup, this provides a way to lazy load it.
nvm() {
  if ! command -v nvm --help >/dev/null; then
    if [ -s "${HOME}/.nvm/nvm.sh" ]; then
      source "${HOME}/.nvm/nvm.sh"
    fi

    if [ -s "/usr/local/opt/nvm/etc/bash_completion.d/nvm" ]; then
      source "/usr/local/opt/nvm/etc/bash_completion.d/nvm"
    fi
  fi

  nvm "${@}"
}