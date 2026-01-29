# FZF fuzzy finder
#
# Configures fzf with custom colors, key bindings, and ripgrep integration.
# Ctrl+W for history search. OS-specific word navigation bindings.

# Make sure fzf's bin is on PATH (Homebrew /usr/local layout)
path_add /usr/local/opt/fzf/bin

has fzf || return

# Ease access of history binding by remapping it.
bindkey -r '^R'
bindkey '^W' fzf-history-widget

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

case $OSTYPE in
  darwin*)
    __fzf_word_binds="alt-left:backward-word,alt-right:forward-word,alt-bs:backward-kill-word,home:first,end:last"
    ;;
  *)
    __fzf_word_binds="ctrl-left:backward-word,ctrl-right:forward-word,ctrl-bs:backward-kill-word,home:first,end:last"
    ;;
esac

export FZF_DEFAULT_OPTS="
  --reverse
  --pointer=''
  --prompt=
  --highlight-line
  --no-separator
  --no-scrollbar
  --info=inline-right
  --color=bg:-1,fg:-1,bg+:#005577,fg+:#eeeeee,hl:#cc5500,hl+:#cc5500,pointer:-1,prompt:-1,info:-1,gutter:-1
  --bind=$__fzf_word_binds"
unset __fzf_word_binds
if has rg; then
  export FZF_DEFAULT_COMMAND='rg --smart-case --files --hidden --glob "!.git/*"'
  export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
fi
