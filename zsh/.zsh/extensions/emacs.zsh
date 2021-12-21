export EMACS='/usr/local/bin/emacs'
export EMACS_DOOM="$HOME/.emacs.doom"

if [[ ! "$PATH" == "*/$EMACS_DOOM/bin*" ]]; then
  export PATH="${PATH:+${PATH}:}$EMACS_DOOM/bin"
fi
