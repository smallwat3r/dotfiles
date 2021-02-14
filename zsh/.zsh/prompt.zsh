# Zsh prompt
# ~~~~~~~~~~

export VIRTUAL_ENV_DISABLE_PROMPT=false
setopt PROMPT_SUBST

_display_git_info() {
  local _git_root=$(echo $(git-root) | sed 's/true/~/')
  local _git_branch=$(echo $(git-branch))
  [[ ! -z $_git_branch ]] && echo " ${_git_branch}${_git_root}"
}

if [[ "$INSIDE_EMACS" ]]; then
  # Emacs shell prompt (do not run with tmux)
  # -----------------------------------------
  PROMPT='%(?..%{$fg[red]%}%? )$resetcolor$(is-venv)$(shpwd)$(_display_git_info) %# '
else
  # Other shell emulators (run tmux by default)
  # Tmux is turn on by default, so I'm using the individual pane titles
  # to display the majority of the prompt information.
  # -------------------------------------------------------------------
  if [ -t 0 ] && [[ -z $TMUX ]] && [[ $- = *i* ]]; then
    exec tmux
  fi

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

  PROMPT='%(?..%{$fg[red]%}%? )$resetcolor$(is-venv)${vim_mode} '

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
fi
