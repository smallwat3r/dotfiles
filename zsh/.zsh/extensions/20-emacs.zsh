export EMACS='/usr/local/bin/emacs'
export EMACS_DOOM="${HOME}/.emacs.doom"

if [[ ! "${PATH}" == "*/${EMACS_DOOM}/bin*" ]]; then
  export PATH="${PATH:+${PATH}:}${EMACS_DOOM}/bin"
fi

if [[ "${INSIDE_EMACS}" = 'vterm' ]] \
  && [[ -n "${EMACS_VTERM_PATH}" ]] \
  && [[ -f "${EMACS_VTERM_PATH}"/etc/emacs-vterm-zsh.sh ]]; then
  source "${EMACS_VTERM_PATH}"/etc/emacs-vterm-zsh.sh
fi

# Utils

clear-cache-emacs-projectile() {
  if [[ -f "${EMACS_DOOM}/.local/cache/projectile.cache" ]]; then
    rm "${EMACS_DOOM}/.local/cache/projectile.cache"
    printf 'Projectile cache has been cleared.\n'
  fi
}

clear-cache-emacs-straight() {
  if read -q 'REPLY?It might be slow to rebuild the packages once cache is cleared. Press Y/y to continue: '; then
    local straight="${EMACS_DOOM}/.local/straight"

    if [[ -d "${straight}/repos/melpa" ]]; then
      git -C "${straight}/repos/melpa" pull
      find "${straight}" -type f -name 'build-*cache.el' -delete
      printf 'Straight cache has been cleared.\n'
    fi
  fi
}
