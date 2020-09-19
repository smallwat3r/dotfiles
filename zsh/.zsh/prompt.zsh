# Zsh prompt
# ~~~~~~~~~~

# Tmux is turn on by default, so I'm using the individual pane titles
# to display the majority of the prompt information.
# -------------------------------------------------------------------

# Launch tmux by default
if [ -t 0 ] && [[ -z $TMUX ]] && [[ $- = *i* ]]; then
  exec tmux
fi

setopt PROMPT_SUBST

vim_ins_mode='%#'
vim_cmd_mode=';;'
vim_mode=$vim_ins_mode

zle-keymap-select() {
  vim_mode="${${KEYMAP/vicmd/${vim_cmd_mode}}/(main|viins)/${vim_ins_mode}}"
  zle reset-prompt
}
zle -N zle-keymap-select

zle-line-finish() {
  vim_mode=$vim_ins_mode
}
zle -N zle-line-finish

export VIRTUAL_ENV_DISABLE_PROMPT=false
PROMPT='%(?..%{$fg[red]%}%? )$resetcolor$(is-venv)${vim_mode} '

_display_git_info() {
  local _git_root=$(echo $(git-root) | sed 's/true/~/')
  local _git_branch=$(echo $(git-branch))
  [[ ! -z $_git_branch ]] && echo " ${_git_branch}${_git_root}"
}

_pane_number() {
  echo $(tmux list-panes | grep "active" | cut -d ':' -f 1)
}

ssh() {
  [[ -z $TMUX ]] ||
    tmux select-pane -t $(_pane_number) -T "#[fg=red,bold]$(echo $* | cut -d . -f 1)#[fg=default]"
  command ssh "$@"
}

precmd() {
  [[ -z $TMUX ]] ||
    tmux select-pane -t $(_pane_number) -T "$(shpwd)$(_display_git_info)"
}
