# .zshenv

if [[ -z "$XDG_CONFIG_HOME" ]]; then
  export XDG_CONFIG_HOME="$HOME/.config"
fi

export PATH='/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin'
export PATH="$HOME/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="/usr/local/opt/python@3.8/bin:$PATH"
export PATH="$HOME/go/bin:$PATH"
export PATH="$HOME/.emacs.doom/bin:$PATH"
export PATH="/Library/TeX/texbin:$PATH"
export PATH="$HOME/flutter/bin:$PATH"
export GOPATH="$HOME/go"

export EDITOR='emacsclient'
export EMACS='/Applications/Emacs.app/Contents/MacOS/Emacs'
