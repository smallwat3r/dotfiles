if [[ ! "${PATH}" == */usr/local/opt/fzf/bin* ]]; then
  export PATH="${PATH:+${PATH}:}/usr/local/opt/fzf/bin"

  # Ease access of history binding by remapping it.
  bindkey -r '^R'
  bindkey '^W' fzf-history-widget
fi

# Fzf provides by default some completion configuration for Zsh.
if [ -f /usr/local/opt/fzf/shell/completion.zsh ]; then
  source '/usr/local/opt/fzf/shell/completion.zsh'
fi

# It also provides some default bindings.
if [ -f /usr/local/opt/fzf/shell/key-bindings.zsh ]; then
  source '/usr/local/opt/fzf/shell/key-bindings.zsh'
fi

export FZF_DEFAULT_OPTS='--reverse --color bg:-1,bg+:-1,fg+:165'
export FZF_DEFAULT_COMMAND='rg --smart-case --files --hidden --glob "!.git/*"'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
