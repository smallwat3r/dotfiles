# ZSH prompt

# Display Python virtual environment name. This function is used in the Zsh prompt.
__is_venv() {
  if [[ ${VIRTUAL_ENV} ]]; then
    echo '%s' "(.${VIRTUAL_ENV##*/}) "
  fi
}

# Disable showing any Python virtual environment information in the shell prompt.
# Indeed I'm using my own function to display this information.
VIRTUAL_ENV_DISABLE_PROMPT=false

# Allow parameter expansion, command substitution and arithmetic expansion in
# the prompt string.
setopt PROMPT_SUBST

# Enable colors.
autoload -U colors && colors

# Display Git information in the prompt. Keep it minimal.
autoload -Uz vcs_info
precmd_vcs_info() {
  vcs_info
}
precmd_functions+=(
  precmd_vcs_info
)
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:*' check-for-changes false  # less expensive
zstyle ':vcs_info:*' formats ' (%F{yellow}%b%f)'

# Manually input custom text in the prompt. Fetch the value of the tag from the
# `_PROMPT_TAG` variable and add custom colors and faces.
__tag() {
  if [ ! -z "${_PROMPT_TAG}" ]; then
    echo "%B%F{87}%K{20}[${(U)_PROMPT_TAG}]%b%f%k "
  fi
}

# Convenience function to define a tag (custom text) in the prompt by setting a value
# for an `_PROMPT_TAG` variable. See __tag private function.
tag() {
  _PROMPT_TAG="${1}"
}

# Prompt format definition. It will print out return codes in red in case the
# command fails.
PROMPT='%(?..%F{red}?%? )$(__tag)$(__is_venv)%F{cyan}%3~%f${vcs_info_msg_0_}%# '

# When outside of emacs, activate tmux by default and use the individual pane
# titles to display the main prompt information.
if [[ ! "${INSIDE_EMACS}" ]]; then
  # Activate tmux
  if [ -t 0 ] && [[ -z "${TMUX}" ]] && [[ $- = *i* ]]; then
    exec tmux
  fi

  # Return the current activate pane number
  __pane_number() {
    tmux list-panes | grep "active" | cut -d ':' -f 1
  }

  # Wrapper around the `ssh` command. It will print out the ssh instance in the
  # active tmux pane number.
  ssh() {
    [[ -z "${TMUX}" ]] \
      || tmux select-pane \
        -t "$(__pane_number)" \
        -T "#[fg=red,bold]$(echo "$*" | cut -d . -f 1)#[fg=default]"
    command ssh "${@}"
  }

  # Return true if the current directory is the root of a git repository.
  __git_root() {
    if [[ $(git rev-parse --is-inside-work-tree 2>/dev/null) == true ]]; then
      [[ $(git rev-parse --show-toplevel 2>/dev/null) == "${PWD}" ]] && echo true
    fi
  }

  # Show a `*` next to the branch name if the Git branch is dirty.
  __git_dirty() {
    [[ $(git diff --shortstat 2>/dev/null | tail -n1) != "" ]] && echo "*"
  }

  # Display current git branch.
  __git_branch() {
    git branch --no-color 2>/dev/null \
      | sed -e '/^[^*]/d' -e "s/* \(.*\)/\1$(__git_dirty)/"
  }

  # Display git information.
  __display_git_info() {
    local _git_root="$(__git_root | sed 's/true/~/')"
    local _git_branch="$(__git_branch)"
    [[ -n ${_git_branch} ]] && echo " ${_git_branch}${_git_root}"
  }

  # Shrink the current directory path.
  # Example: ~/foo/bar/hello.py would become ~/f/b/hello.py
  __shrink_path() {
    echo ~+ \
      | sed "s;${HOME};~;" \
      | sed 's;\(/.\)[^/]*;\1;g' \
      | sed 's/.$//'
  }

  # Print current directory path.
  __path() {
    case "${PWD}" in
      "${HOME}") printf '~' ;;
      "/"      ) printf '/' ;;
      *        ) printf '%s%s' "$(__shrink_path)" "${PWD##*/}" ;;
    esac
  }

  # Run whenever the Zsh prompt is reloaded. Update the information in the
  # active Tmux pane session title.
  precmd() {
    [[ -z "${TMUX}" ]] \
      || tmux select-pane \
        -t "$(__pane_number)" \
        -T "$(__path)$(__display_git_info)"
  }
fi