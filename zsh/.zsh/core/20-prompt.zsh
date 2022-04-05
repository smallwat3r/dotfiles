# ZSH prompt

__is_venv() {
  if [[ ${VIRTUAL_ENV} ]]; then
    echo '%s' "(.${VIRTUAL_ENV##*/}) "
  fi
}

VIRTUAL_ENV_DISABLE_PROMPT=false
setopt PROMPT_SUBST

autoload -U colors && colors
autoload -Uz vcs_info

precmd_vcs_info() {
  vcs_info
}
precmd_functions+=(
  precmd_vcs_info
)

zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:*' check-for-changes false  # less expensive
zstyle ':vcs_info:*' formats ' (%b)'

# Placeholder to manually input custom text in prompt, set __=XXX in the shell
__placeholder() {
  if [ ! -z "${__}" ]; then
    echo "%B%F{87}%K{20}[${(U)__}]%b%f%k "
  fi
}

PROMPT='%(?..%F{red}?%? )$(__placeholder)$(__is_venv)%F{cyan}%2~%f${vcs_info_msg_0_} %# '

# When outside of emacs, activate tmux by default and use the individual pane titles
# to display the main prompt information.
if [[ ! "${INSIDE_EMACS}" ]]; then
  if [ -t 0 ] && [[ -z "${TMUX}" ]] && [[ $- = *i* ]]; then
    exec tmux
  fi

  __pane_number() {
    tmux list-panes | grep "active" | cut -d ':' -f 1
  }

  ssh() {
    [[ -z "${TMUX}" ]] \
      || tmux select-pane \
        -t "$(__pane_number)" \
        -T "#[fg=red,bold]$(echo "$*" | cut -d . -f 1)#[fg=default]"
    command ssh "${@}"
  }

  __git_dirty() {
    [[ $(git diff --shortstat 2>/dev/null | tail -n1) != "" ]] && echo "*"
  }

  __git_root() {
    if [[ $(git rev-parse --is-inside-work-tree 2>/dev/null) == true ]]; then
      [[ $(git rev-parse --show-toplevel 2>/dev/null) == "${PWD}" ]] && echo true
    fi
  }

  __git_branch() {
    git branch --no-color 2>/dev/null \
      | sed -e '/^[^*]/d' -e "s/* \(.*\)/\1$(__git_dirty)/"
  }

  __display_git_info() {
    local _git_root="$(__git_root | sed 's/true/~/')"
    local _git_branch="$(__git_branch)"
    [[ -n ${_git_branch} ]] && echo " ${_git_branch}${_git_root}"
  }

  __shrink_path() {
    echo ~+ \
      | sed "s;${HOME};~;" \
      | sed 's;\(/.\)[^/]*;\1;g' \
      | sed 's/.$//'
  }

  __path() {
    case "${PWD}" in
      "${HOME}") printf '~' ;;
      "/"      ) printf '/' ;;
      *        ) printf '%s%s' "$(__shrink_path)" "${PWD##*/}" ;;
    esac
  }

  precmd() {
    [[ -z "${TMUX}" ]] \
      || tmux select-pane \
        -t "$(__pane_number)" \
        -T "$(__path)$(__display_git_info)"
  }
fi
