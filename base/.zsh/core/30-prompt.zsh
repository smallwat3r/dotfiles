# Prompt configuration
#
# Minimal prompt with git status, virtualenv indicator, and optional
# session tags. Also auto-starts tmux for interactive terminal sessions
# (unless inside Emacs or Hammerspoon).
#
# All dynamic segments are computed in a precmd hook and stored in
# variables, avoiding subshell forks on each prompt render.

# Disable the default virtualenv prompt modification. Python's venv
# activation script prepends "(venv)" to PS1, but we handle this
# ourselves for consistent styling.
VIRTUAL_ENV_DISABLE_PROMPT=1

setopt PROMPT_SUBST

# Session tags for prompt labeling. Set via `tag "label"`, clear
# with `tag`.
tag() { _PROMPT_TAG="$1" }

__prompt_precmd() {
  local last_status=$?

  __ps_err='' __ps_tag='' __ps_venv='' __ps_git=''

  # exit status
  (( last_status )) && __ps_err="%F{red}?${last_status} "

  # session tag
  [[ -n $_PROMPT_TAG ]] && \
    __ps_tag="%B%F{87}%K{20}[${(U)_PROMPT_TAG}]%b%f%k "

  # virtualenv
  (( ${+VIRTUAL_ENV} )) && \
    __ps_venv="venv(${VIRTUAL_ENV##*/}) "

  # git
  local gstatus branch dirty root paused

  gstatus=$(
    GIT_OPTIONAL_LOCKS=0 \
    git status --porcelain=v2 -b \
      --no-ahead-behind 2>/dev/null
  ) || return

  branch=${gstatus#*branch.head }
  branch=${branch%%$'\n'*}
  [[ -z $branch || $branch == "# "* ]] && return

  [[ $gstatus == *$'\n'[^#]* ]] && dirty='*'
  [[ -e .git ]] && root='~'

  local subject
  subject=$(git log -1 --format=%s 2>/dev/null)
  [[ $subject == PAUSED* ]] && \
    paused=' %B%F{198}%K{52}[PAUSED]%b%f%k'

  __ps_git="${paused}%F{yellow} (${branch}${dirty}${root})%f "
}

autoload -Uz add-zsh-hook
add-zsh-hook precmd __prompt_precmd

PROMPT='${__ps_err}${__ps_tag}${__ps_venv}%f%3~%f${__ps_git}%# '

# Auto-start tmux for interactive terminal sessions, unless inside
# Emacs (which has its own window management) or Hammerspoon (macOS
# automation).
# Conditions: stdin is a tty, not already in tmux, shell is
# interactive.
if (( ! ${+INSIDE_EMACS} && ! ${+INSIDE_HS} )); then
  if [[ -t 0 && -z "$TMUX" && $- == *i* ]]; then
    exec tmux
  fi
fi
