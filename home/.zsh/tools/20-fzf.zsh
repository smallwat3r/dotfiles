# FZF fuzzy finder
#
# Configures fzf with custom colors, key bindings, and ripgrep integration.
# Ctrl+W for history search.

has fzf || return

__fzf_source_first() {
  local f
  for f in "$@"; do
    [[ -r $f ]] && source "$f" && return 0
  done
}

__fzf_source_first \
  /usr/share/fzf/completion.zsh \
  /usr/share/fzf/shell/completion.zsh
__fzf_source_first \
  /usr/share/fzf/key-bindings.zsh \
  /usr/share/fzf/shell/key-bindings.zsh

# Ease access of history binding by remapping it. Must run after
# key-bindings.zsh is sourced, which binds ^R and defines the widget.
bindkey -r '^R'
bindkey '^W' fzf-history-widget

export FZF_DEFAULT_OPTS="
  --reverse
  --pointer=''
  --prompt=
  --highlight-line
  --no-separator
  --no-scrollbar
  --info=inline-right
  --color=bg:-1,fg:-1,bg+:$THEME_FOCUS,fg+:$THEME_WHITE,hl:$THEME_ACCENT,hl+:$THEME_ACCENT,pointer:-1,prompt:-1,info:-1,gutter:-1
  --bind=ctrl-left:backward-word,ctrl-right:forward-word,ctrl-bs:backward-kill-word,home:first,end:last"
if has rg; then
  export FZF_DEFAULT_COMMAND='rg --smart-case --files --hidden --glob "!.git/*"'
  export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
fi
