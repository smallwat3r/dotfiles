# Vim bindings emulation in Zsh
# Activate it only when running outside of Emacs, as it can conflicts with Emacs
# internal bindings, specially when using Evil mode

if [[ ! "${INSIDE_EMACS}" || "${INSIDE_EMACS}" = 'alacritty' ]]; then
  setopt VI

  # Use `jk` as ESC
  export KEYTIMEOUT=20
  bindkey -M viins 'jk' vi-cmd-mode

  # Fix deletion after switching modes
  bindkey '^?' backward-delete-char

  # Copy behaviour
  __vi_yank_pbcopy() {
    zle vi-yank
    echo "${CUTBUFFER}" | pbcopy
  }

  zle -N __vi_yank_pbcopy
  bindkey -M vicmd 'y' __vi_yank_pbcopy

  # Manage cursor shape from insert and normal modes
  zle-keymap-select() {
    if [[ "${KEYMAP}" == vicmd ]] || [[ $1 = 'block' ]]; then
      echo -ne '\e[1 q'
    elif [[ "${KEYMAP}" == main ]] || [[ "${KEYMAP}" == viins ]] || [[ "${KEYMAP}" = '' ]] || [[ "${1}" = 'beam' ]]; then
      echo -ne '\e[5 q'
    fi
  }
  zle -N zle-keymap-select

  echo -ne '\e[5 q'  # default cursor

  __fix_cursor() {
    echo -ne '\e[5 q'
  }

  precmd_functions+=(
    __fix_cursor
  )
fi
