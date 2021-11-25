export NVM_DIR="$HOME/.nvm"

# I find nvm quite bulky in my zsh config, so this function provides a way to manually lazy load nvm
# when I need to use it.
load-nvm() {
  [ -s "/usr/local/opt/nvm/nvm.sh" ] && source "/usr/local/opt/nvm/nvm.sh"
  [ -s "/usr/local/opt/nvm/etc/bash_completion.d/nvm" ] && source "/usr/local/opt/nvm/etc/bash_completion.d/nvm"
}
