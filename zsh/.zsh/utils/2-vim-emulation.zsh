# Vim bindings emulation in Zsh

# Make it only if outside of Emacs, as it sometimes conflicts with Emacs
# internal commands, specially when using Evil mode
if [[ ! "${INSIDE_EMACS}" ]]; then
  setopt VI  # same as 'bindkey -v'

  export KEYTIMEOUT=20  # need at least 20 for jk binding to work properly
  bindkey -M viins 'jk' vi-cmd-mode

  bindkey '^?' backward-delete-char

  # yank to clipboard
  __vi_yank_pbcopy() {
    zle vi-yank
    echo "${CUTBUFFER}" | pbcopy
  }
  zle -N __vi_yank_pbcopy
  bindkey -M vicmd 'y' __vi_yank_pbcopy

  # Edit command in preferred editor
  autoload edit-command-line
  zle -N edit-command-line
  bindkey '^e' edit-command-line
fi
