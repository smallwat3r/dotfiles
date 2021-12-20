if [ -d '/usr/local/opt/fzf' ]; then
  if [[ ! "$PATH" == */usr/local/opt/fzf/bin* ]]; then
    export PATH="${PATH:+${PATH}:}/usr/local/opt/fzf/bin"
  fi

  [[ $- == *i* ]] && source '/usr/local/opt/fzf/shell/completion.zsh' 2>/dev/null

  source '/usr/local/opt/fzf/shell/key-bindings.zsh'

  export FZF_DEFAULT_OPTS=$FZF_DEFAULT_OPTS'
    --color=fg:#d0d0d0,bg:#000000,hl:#ffd966
    --color=fg+:#d0d0d0,bg+:#000000,hl+:#daa915
    --color=info:#d0d0d0,prompt:#d0d0d0,pointer:#d0d0d0
    --color=marker:#d0d0d0,spinner:#d0d0d0,header:#d0d0d0'
fi
