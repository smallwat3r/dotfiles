if [[ ! "${PATH}" == */usr/local/opt/fzf/bin* ]]; then
  export PATH="${PATH:+${PATH}:}/usr/local/opt/fzf/bin"
fi

if [ -f /usr/local/opt/fzf/shell/completion.zsh ]; then
  source '/usr/local/opt/fzf/shell/completion.zsh'
fi

if [ -f /usr/local/opt/fzf/shell/key-bindings.zsh ]; then
  source '/usr/local/opt/fzf/shell/key-bindings.zsh'
fi


export FZF_DEFAULT_OPTS='
  --height 40% --reverse
  --color fg:242,bg:232,hl:65,fg+:15,bg+:232,hl+:108
  --color info:242,prompt:242,spinner:108,pointer:242,marker:168
'
export FZF_DEFAULT_COMMAND='rg --smart-case --files --hidden --glob "!.git/*"'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
