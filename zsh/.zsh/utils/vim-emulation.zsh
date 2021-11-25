export KEYTIMEOUT=20 # if the keytimeout was too short, jk wouldn't work for ESC

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
