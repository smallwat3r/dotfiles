# ZSH prompt

# Display Python virtual environment name. This function is used in the Zsh prompt.
__is_venv() {
  if (( ${+VIRTUAL_ENV} )); then
    printf '%s' "venv(${VIRTUAL_ENV##*/}) "
  fi
}

# Consolidated git info for the prompt:
# - branch name
# - '*' if dirty
# - '~' if at repo root
# - [PAUSED] badge if last commit subject starts with "PAUSED"
__git_prompt_segment() {
  local git_status first_line branch rest dirty top root paused

  # One status call to rule them all
  git_status=$(git status --porcelain=v1 -b 2>/dev/null) || return

  # First line looks like: "## main" or "## main...origin/main [ahead 1]"
  first_line=${git_status%%$'\n'*}
  first_line=${first_line#\#\# }

  # Extract branch name before "..." or space
  branch=${first_line%%...*}
  branch=${branch%% *}

  # Dirty if there is more than one line in porcelain output
  rest=${git_status#*$'\n'}
  [[ -n $rest ]] && dirty='*' || dirty=''

  # Repo root marker (~) if we're at the top-level dir
  top=$(git rev-parse --show-toplevel 2>/dev/null) || top=''
  if [[ -n $top && $top == ${PWD:A} ]]; then
    root='~'
  else
    root=''
  fi

  # PAUSED badge if last commit subject starts with "PAUSED"
  if git log -1 --format="%s" 2>/dev/null | grep -q '^PAUSED'; then
    paused=' %B%F{198}%K{52}[PAUSED]%b%f%k'
  else
    paused=''
  fi

  # Emit paused badge + yellow git info (branch[*]~)
  # % codes are prompt escapes and will be interpreted because PROMPT_SUBST is set.
  printf '%s%%F{yellow} (%s%s%s)%%f ' "$paused" "$branch" "$dirty" "$root"
}

# Disable showing any Python virtual environment information in the shell prompt.
# We are using our own function for this.
VIRTUAL_ENV_DISABLE_PROMPT=1

setopt PROMPT_SUBST
autoload -U colors && colors

# Manually input custom text in the prompt. Fetch the value of the tag from the
# `_PROMPT_TAG` variable and add custom colors and faces.
__tag() {
  if [[ -n "${_PROMPT_TAG}" ]]; then
    echo "%B%F{87}%K{20}[${(U)_PROMPT_TAG}]%b%f%k "
  fi
}

# Convenience function to define a tag (custom text) in the prompt by setting a value
# for a `_PROMPT_TAG` variable. See __tag private function.
tag() {
  _PROMPT_TAG="$1"
}

PROMPT='%(?..%F{red}?%? )$(__tag)$(__is_venv)%f%3~%f$(__git_prompt_segment)%# '

# When outside of emacs, activate tmux by default and use the individual pane
# titles to display the main prompt information.
if ((! ${+INSIDE_EMACS} )) && ((! ${+INSIDE_HS})); then
  # Activate tmux
  if [ -t 0 ] && [[ -z "${TMUX}" ]] && [[ $- = *i* ]]; then
    exec tmux
  fi
fi
