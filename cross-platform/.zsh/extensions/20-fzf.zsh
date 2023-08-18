# FZF configs and helper functions
# Dependencies: fzf

if [[ ! "${PATH}" == */usr/local/opt/fzf/bin* ]]; then
  export PATH="${PATH:+${PATH}:}/usr/local/opt/fzf/bin"
fi

if (( $+commands[fzf] )); then
  # Ease access of history binding by remapping it.
  bindkey -r '^R'
  bindkey '^W' fzf-history-widget
fi

# Loop over an array of possible config location, and source the file
# if one exists.
__load_fzf_config() {
  local config_paths=("$@")
  local config
  for config ("${config_paths[@]}"); do
    if [ -f "${config}" ]; then
      source "${config}" && break
    fi
  done
}

# Fzf provides by default some completion configuration for Zsh.
__fzf_possible_completion_config_paths=(
  '/usr/local/opt/fzf/shell/completion.zsh'
  '/usr/share/fzf/completion.zsh'
  '/opt/homebrew/opt/fzf/shell/completion.zsh'
)

# It also provides some default bindings.
__fzf_possible_keybinding_config_paths=(
  '/usr/local/opt/fzf/shell/key-bindings.zsh'
  '/usr/share/fzf/key-bindings.zsh'
  '/opt/homebrew/opt/fzf/shell/key-bindings.zsh'
)

__load_fzf_config ${__fzf_possible_completion_config_paths}
__load_fzf_config ${__fzf_possible_keybinding_config_paths}

export FZF_DEFAULT_OPTS='--reverse --color bg:-1,bg+:-1,fg+:184'
export FZF_DEFAULT_COMMAND='rg --smart-case --files --hidden --glob "!.git/*"'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
