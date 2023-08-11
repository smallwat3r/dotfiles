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

if [ -d /opt/homebrew/bin ]; then
  # this needs to leave here as modules from config depends on this
  export PATH="${PATH:+${PATH}:}/opt/homebrew/bin"
fi

if [[ $(uname) == 'Darwin' ]]; then
  export TERMINAL='alacritty'
else
  export TERMINAL='st'
fi

export EDITOR="${HOME}/.local/bin/ec"

export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8


if [[ "${INSIDE_EMACS}" = 'vterm' ]]; then
  export TERM=eterm-color
else
  export TERM=xterm-256color
fi

export CLICOLOR=1
export GREP_OPTIONS='--color=auto'
export GREP_COLOR='0;31'

# Load private env configs
if [[ -f "${HOME}/.zshenv.private" ]]; then
  source "${HOME}/.zshenv.private"
fi
