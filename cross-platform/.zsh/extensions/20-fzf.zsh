# FZF configs and helper functions
# Dependencies: fzf

# Make sure fzf's bin is on PATH (Homebrew /usr/local layout)
if (( ${path[(Ie)/usr/local/opt/fzf/bin]} == 0 )); then
  path+=("/usr/local/opt/fzf/bin")
fi

if (( $+commands[fzf] )); then
  # ease access of history binding by remapping it.
  bindkey -r '^R'
  bindkey '^W' fzf-history-widget
fi

# Loop over an array of possible config locations, and source the first that exists.
__load_fzf_config() {
  local config
  for config in "$@"; do
    [[ -f $config ]] && source "$config" && break
  done
}

# Fzf provides by default some completion configuration for Zsh.
__fzf_possible_completion_config_paths=(
  '/usr/local/opt/fzf/shell/completion.zsh'
  '/usr/share/fzf/completion.zsh'
  '/opt/homebrew/opt/fzf/shell/completion.zsh'
)

# it also provides some default bindings
__fzf_possible_keybinding_config_paths=(
  '/usr/share/fzf/shell/key-bindings.zsh'
  '/usr/local/opt/fzf/shell/key-bindings.zsh'
  '/usr/share/fzf/key-bindings.zsh'
  '/opt/homebrew/opt/fzf/shell/key-bindings.zsh'
)

__load_fzf_config "${__fzf_possible_completion_config_paths[@]}"
__load_fzf_config "${__fzf_possible_keybinding_config_paths[@]}"

export FZF_DEFAULT_OPTS='--reverse --color bg:-1,bg+:-1,fg+:186,hl:115,hl+:115'
if (( $+commands[rg] )); then
  export FZF_DEFAULT_COMMAND='rg --smart-case --files --hidden --glob "!.git/*"'
  export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
fi
