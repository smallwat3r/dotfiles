# Emacs configuration
#
# Eat terminal integration for shell features like directory tracking.

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
