# Emacs configs and helper functions

# Prefer a Homebrew Emacs on macOS, otherwise fall back to /usr/bin/emacs
if [[ $(uname) == Darwin ]]; then
  if [[ -x /opt/homebrew/bin/emacs ]]; then
    export EMACS='/opt/homebrew/bin/emacs'
  else
    export EMACS='emacs'
  fi
else
  export EMACS='/usr/bin/emacs'
fi

export EMACS_DOOM="$HOME/.emacs.d"

# lsp-mode can be compiled in 2 modes: plist- and hash-table-based.
# plists provide better performance and less pressure than hash-tables.
# NOTE: lsp-mode related packages may need to be reinstalled after changing this.
export LSP_USE_PLISTS=true

# Ensure Doom's bin directory is on PATH
if (( ${path[(Ie)$EMACS_DOOM/bin]} == 0 )); then
  path+=("$EMACS_DOOM/bin")
fi

# Vterm is used as a terminal emulator within Emacs. It provides by default some
# useful functions and configurations for Zsh.
if [[ $INSIDE_EMACS == 'vterm' ]] \
  && [[ -n $EMACS_VTERM_PATH ]] \
  && [[ -f $EMACS_VTERM_PATH/etc/emacs-vterm-zsh.sh ]]; then
  source "$EMACS_VTERM_PATH/etc/emacs-vterm-zsh.sh"
fi

# Avoid using vim from inside Emacs due to conflicting bindings.
if [[ $INSIDE_EMACS == 'vterm' ]] || [[ $INSIDE_EMACS == '28.2,eshell' ]]; then
  alias \
    v="echo 'Do not open vim from Emacs'" \
    vi="echo 'Do not open vim from Emacs'" \
    vim="echo 'Do not open vim from Emacs'" \
    nvim="echo 'Do not open vim from Emacs'"
fi

# Projectile builds a cache that sometimes needs to be cleared for Emacs to see
# new files and directories. This clears the cache for all projectile projects.
clear-cache-emacs-projectile() {
  local cache="$EMACS_DOOM/.local/cache/projectile.cache"

  if [[ -f $cache ]]; then
    rm -- "$cache"
    printf 'Projectile cache has been cleared.\n'
  fi
}

# Straight is a powerful Emacs package manager. It builds a cache of the Emacs
# package versions currently installed. This clears that cache.
# Note: rebuilding packages afterwards can be slow.
clear-cache-emacs-straight() {
  if read -q 'REPLY?It might be slow to rebuild the packages once cache is cleared. Press Y/y to continue: '; then
    printf '\n'
    local straight="$EMACS_DOOM/.local/straight"

    if [[ -d $straight/repos/melpa ]]; then
      git -C "$straight/repos/melpa" pull
      find "$straight" -type f -name 'build-*cache.el' -delete
      printf 'Straight cache has been cleared.\n'
    else
      printf 'No melpa repo found at %s/repos/melpa\n' "$straight"
    fi
  else
    printf '\nAborted.\n'
  fi
}
