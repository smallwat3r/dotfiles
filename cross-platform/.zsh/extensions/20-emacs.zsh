# Emacs configs and helper functions

if [[ $(uname) == 'Darwin' ]]; then
  export EMACS='/opt/homebrew/bin/emacs'
else
  export EMACS='/usr/bin/emacs'
fi

export EMACS_DOOM="${HOME}/.emacs.d"

# lsp-mode can be compiled in 2 modes plist and hash-table based lsp-use-plists flag.
# plists provide better performance in deserialization and also put less presure than
# hash-tables.
# NOTE lsp-mode related packages need to uninstalled and reinstalled again.
export LSP_USE_PLISTS=true

if [[ ! "${PATH}" == "*/${EMACS_DOOM}/bin*" ]]; then
  export PATH="${PATH:+${PATH}:}${EMACS_DOOM}/bin"
fi

# Vterm is used as a terminal emulator within Emacs. It provides by default some
# useful functions and configurations for Zsh.
if [[ "${INSIDE_EMACS}" = 'vterm' ]] \
  && [[ -n "${EMACS_VTERM_PATH}" ]] \
  && [[ -f "${EMACS_VTERM_PATH}"/etc/emacs-vterm-zsh.sh ]]; then
  source "${EMACS_VTERM_PATH}"/etc/emacs-vterm-zsh.sh
fi

# Avoid using vim from inside Emacs due to conflicting bindings.
if [[ "${INSIDE_EMACS}" = 'vterm' ]] || [[ "${INSIDE_EMACS}" = '28.2,eshell' ]]; then
  alias \
    v="echo 'Do not open vim from Emacs'" \
    vi="echo 'Do not open vim from Emacs'" \
    vim="echo 'Do not open vim from Emacs'" \
    nvim="echo 'Do not open vim from Emacs'"
fi

# Utils

# Projectile builds a cache that sometimes needs to be cleared for Emacs to get
# updates of new files and directories. This command clears this cache for all
# the projectile projects.
clear-cache-emacs-projectile() {
  if [[ -f "${EMACS_DOOM}/.local/cache/projectile.cache" ]]; then
    rm "${EMACS_DOOM}/.local/cache/projectile.cache"
    printf 'Projectile cache has been cleared.\n'
  fi
}

# Straight is a powerful Emacs package manager. It builds a cache of the Emacs
# package versions currently installed on the system. This commands clears this
# cache. Note that it can take quite some time to run and that all the packages
# installed would need to be rebuild. This is sometimes useful when upgrading
# and keeping Emacs up-to-date.
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
