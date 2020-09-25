# .zshenv
# ~~~~~~~

export TERM='xterm-256color'

export LANGUAGE=en_US.UTF-8
export LANG=en_US.UTF-8
export LC_CTYPE=en_US.UTF-8
export LC_ALL=en_US.UTF-8

if [[ -z "$XDG_CONFIG_HOME" ]]; then
  export XDG_CONFIG_HOME="$HOME/.config"
fi

export PATH='/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin'
export PATH="$HOME/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="/usr/local/opt/python@3.8/bin:$PATH"
export PATH="$HOME/go/bin:$PATH"
export PATH="$HOME/.emacs.d/bin:$PATH"
export GOPATH="$HOME/go"

export CLICOLOR=1

export EDITOR='/usr/local/bin/nvim'

export LDFLAGS='-L/usr/local/opt/python@3.8/lib'
export PER5LIB="$HOME/lib/perl5"

export HISTFILE="$HOME/.zsh_history"
export HISTSIZE=999999999
export SAVEHIST=$HISTSIZE

export KEYTIMEOUT=1

export GREP_OPTIONS='--color=auto'
export GREP_COLOR='0;30;42'
