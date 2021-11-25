setopt VI  # Emulate vim mode in zsh (same as 'bindkey -v')

export KEYTIMEOUT=20  # need at least 20 for jk binding to work properly
bindkey -M viins 'jk' vi-cmd-mode

bindkey '^?' backward-delete-char

# yank to clipboard
_vi_yank_pbcopy() {
  zle vi-yank
  echo "$CUTBUFFER" | pbcopy
}
zle -N _vi_yank_pbcopy
bindkey -M vicmd 'y' _vi_yank_pbcopy

# Edit command in preferred editor
autoload edit-command-line
zle -N edit-command-line
bindkey '^e' edit-command-line
