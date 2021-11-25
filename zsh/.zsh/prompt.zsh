__git_root() {
  if [[ $(git rev-parse --is-inside-work-tree 2>/dev/null) == true ]]; then
    [[ $(git rev-parse --show-toplevel 2>/dev/null) == "$PWD" ]] && echo true
  fi
}

__git_dirty() {
  [[ $(git diff --shortstat 2>/dev/null | tail -n1) != "" ]] && echo "*"
}

__git_branch() {
  git branch --no-color 2>/dev/null \
    | sed -e '/^[^*]/d' -e "s/* \(.*\)/\1$(__git_dirty)/"
}

__display_git_info() {
  local _git_root
  local _git_branch
  _git_root="$(__git_root | sed 's/true/~/')"
  _git_branch="$(__git_branch)"
  [[ -n $_git_branch ]] && echo " ${_git_branch}${_git_root}"
}

__shrink_path() {
  echo ~+ \
    | sed "s;$HOME;~;" \
    | sed 's;\(/.\)[^/]*;\1;g' \
    | sed 's/.$//'
}

__path() {
  case $PWD in
    "$HOME") printf '~' ;;
    *) printf '%s%s' "$(__shrink_path)" "${PWD##*/}" ;;
  esac
}

__is_venv() {
  if [[ $VIRTUAL_ENV ]]; then
    echo '%s' "(.${VIRTUAL_ENV##*/}) "
  fi
}

VIRTUAL_ENV_DISABLE_PROMPT=false
setopt PROMPT_SUBST

# If we are from Emacs, run a standard prompt and do not run tmux. Else, in other shell emulators
# activate tmux by default and use the individual pane titles to display the main prompt information.
if [[ "$INSIDE_EMACS" ]]; then
  PROMPT='%(?..%? )$(__is_venv)$(__path)$(__display_git_info) %# '
else
  if [ -t 0 ] && [[ -z $TMUX ]] && [[ $- = *i* ]]; then
    exec tmux
  fi

  PROMPT='%(?..%? )$(__is_venv)%# '

  __pane_number() {
    tmux list-panes | grep "active" | cut -d ':' -f 1
  }

  ssh() {
    [[ -z $TMUX ]] \
      || tmux select-pane \
        -t "$(__pane_number)" \
        -T "#[fg=red,bold]$(echo "$*" | cut -d . -f 1)#[fg=default]"
    command ssh "$@"
  }

  precmd() {
    [[ -z $TMUX ]] \
      || tmux select-pane \
        -t "$(__pane_number)" \
        -T "$(__path)$(__display_git_info)"
  }
fi