# Emacs configuration
#
# Sets up Emacs paths, Doom Emacs bin directory, and Eat terminal
# integration for shell features like directory tracking.

# Prefer a Homebrew Emacs on macOS, otherwise fall back to /usr/bin/emacs
if is_macos; then
  if [[ -x /opt/homebrew/bin/emacs ]]; then
    export EMACS='/opt/homebrew/bin/emacs'
  else
    export EMACS='emacs'
  fi
else
  export EMACS='/usr/bin/emacs'
fi

# Eat terminal integration (directory tracking, command tracking)
# Skip on TRAMP remotes (TERM won't be eat-*) and in child shells
# (e.g. timezsh) where re-initializing Eat causes hangs.
if [[ $TERM == eat-* && -z $__EAT_LOADED ]]; then
  export __EAT_LOADED=1
  for __eat_f in ~/.emacs.d/elpaca/repos/eat/integration/zsh \
                 ~/.emacs.d/elpa/eat-*/integration/zsh(N[-1]); do
    [[ -r $__eat_f ]] && source "$__eat_f" && break
  done
  unset __eat_f
fi
