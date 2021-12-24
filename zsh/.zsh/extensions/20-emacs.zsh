export EMACS='/usr/local/bin/emacs'
export EMACS_DOOM="$HOME/.emacs.doom"

if [[ ! "${PATH}" == "*/${EMACS_DOOM}/bin*" ]]; then
  export PATH="${PATH:+${PATH}:}${EMACS_DOOM}/bin"
fi

if [[ "$INSIDE_EMACS" = 'vterm' ]] && [[ -n "${EMACS_VTERM_PATH}" ]] && [[ -f "${EMACS_VTERM_PATH}"/etc/emacs-vterm-zsh.sh ]]; then
  source "${EMACS_VTERM_PATH}"/etc/emacs-vterm-zsh.sh
fi
