# .zshenv

if [[ -z "${XDG_CONFIG_HOME}" ]]; then
  export XDG_CONFIG_HOME="${HOME}/.config"
fi

typeset -U PATH path cdpath manpath

path=(
  /usr/local/bin
  /usr/local/sbin
  /usr/bin
  /bin
  /sbin
  /usr/sbin
  "${HOME}"/.local/bin
)

cdpath=("${HOME}")

manpath=(
  /usr/local/share/man
  /usr/share/man
)

export PATH

if [ -d /opt/homebrew ]; then
  # this needs to live here as modules from config depends on this
  export PATH="/opt/homebrew/bin:$PATH"
  export PATH="/opt/homebrew/sbin:$PATH"
  export PATH="/opt/homebrew/opt/sphinx-doc/bin:$PATH"
fi

if [ -d "$HOME/.npm-global/bin" ]; then
  export PATH="$HOME/.npm-global/bin:$PATH"
fi

if [[ $(uname) == 'Darwin' ]]; then
  export PATH="/Applications/Alacritty.app/Contents/MacOS:$PATH"
  export TERMINAL='alacritty'
else
  export TERMINAL='st'
fi

export EDITOR="${HOME}/.local/bin/ec"

export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8

export CLICOLOR=1

# Load private env configs
if [[ -f "${HOME}/.zshenv.private" ]]; then
  source "${HOME}/.zshenv.private"
fi
