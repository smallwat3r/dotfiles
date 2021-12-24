# Vim bindings emulation in Zsh
# Activate it only when running outside of Emacs, as it can conflicts with Emacs
# internal bindings, specially when using Evil mode

if [[ ! "${INSIDE_EMACS}" ]]; then
  setopt VI

  # Use `jk` as ESC
  export KEYTIMEOUT=20
  bindkey -M viins 'jk' vi-cmd-mode

  __vi_yank_pbcopy() {
    zle vi-yank
    echo "${CUTBUFFER}" | pbcopy
  }

  zle -N __vi_yank_pbcopy
  bindkey -M vicmd 'y' __vi_yank_pbcopy
fi
