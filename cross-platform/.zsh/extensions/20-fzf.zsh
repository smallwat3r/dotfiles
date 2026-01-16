# FZF configs and helper functions
# Dependencies: fzf

# Make sure fzf's bin is on PATH (Homebrew /usr/local layout)
if [[ -d /usr/local/opt/fzf/bin ]] && (( ${path[(Ie)/usr/local/opt/fzf/bin]} == 0 )); then
  path+=("/usr/local/opt/fzf/bin")
fi

if (( $+commands[fzf] )); then
  # ease access of history binding by remapping it.
  bindkey -r '^R'
  bindkey '^W' fzf-history-widget
fi

__fzf_source_first() {
  local f
  for f in "$@"; do
    [[ -r $f ]] && source "$f" && return 0
  done
}

__load_fzf_config() {
  local -a completion_paths keybinding_paths

  case $OSTYPE in
    darwin*)
      completion_paths=(
        /opt/homebrew/opt/fzf/shell/completion.zsh
        /usr/local/opt/fzf/shell/completion.zsh
      )
      keybinding_paths=(
        /opt/homebrew/opt/fzf/shell/key-bindings.zsh
        /usr/local/opt/fzf/shell/key-bindings.zsh
      )
      ;;
    linux*)
      completion_paths=(
        /usr/share/fzf/completion.zsh
        /usr/share/fzf/shell/completion.zsh
      )
      keybinding_paths=(
        /usr/share/fzf/key-bindings.zsh
        /usr/share/fzf/shell/key-bindings.zsh
      )
      ;;
    *)
      completion_paths=( "$HOME/.fzf.zsh" )
      keybinding_paths=()
      ;;
  esac

  __fzf_source_first "${completion_paths[@]}"
  __fzf_source_first "${keybinding_paths[@]}"
}

__load_fzf_config

export FZF_DEFAULT_OPTS='
  --reverse
  --color=bg:-1,bg+:-1
  --color=fg:-1,fg+:-1
  --color=hl:33,hl+:33
  --color=info:30
  --color=prompt:30
  --color=pointer:32
  --color=marker:32
  --color=spinner:32
'
if (( $+commands[rg] )); then
  export FZF_DEFAULT_COMMAND='rg --smart-case --files --hidden --glob "!.git/*"'
  export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
fi
