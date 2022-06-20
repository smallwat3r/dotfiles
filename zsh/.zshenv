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
  /usr/sbin
  "${HOME}"/.local/bin
)

cdpath=("${HOME}")

manpath=(
  /usr/local/share/man
  /usr/share/man
)

export PATH

export EDITOR='emacsclient'

export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8

export TERM='xterm-256color'
export CLICOLOR=1

export GREP_OPTIONS='--color=auto'
export GREP_COLOR='0;31'

export HISTFILE="${HOME}/.zsh_history"
export HISTSIZE=999999999
export SAVEHIST="${HISTSIZE}"

if [[ -f "${HOME}/.zshenv.private" ]]; then
  source "${HOME}/.zshenv.private"
fi
