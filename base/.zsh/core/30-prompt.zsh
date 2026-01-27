# ZSH prompt

# Display Python virtual environment name.
__is_venv() {
  if (( ${+VIRTUAL_ENV} )); then
    printf '%s' "venv(${VIRTUAL_ENV##*/}) "
  fi
}

# Git prompt segment showing:
#   - branch name
#   - * if working tree has uncommitted changes
#   - ~ if current directory is the repository root
#   - [PAUSED] badge if last commit subject starts with "PAUSED" (for git-pause workflow)
#
# Uses porcelain v2 format for reliable parsing. All git calls run in a single
# subshell with NUL separators to minimize fork overhead. --no-optional-locks
# prevents git from acquiring locks, avoiding delays on busy repos.
__git_prompt_segment() {
  local info gstatus prefix subject branch dirty root paused

  info=$(
    git --no-optional-locks status --porcelain=v2 -b 2>/dev/null
    printf '\0'
    git --no-optional-locks rev-parse --show-prefix 2>/dev/null
    printf '\0'
    git --no-optional-locks log -1 --format=%s 2>/dev/null
  ) || return

  # Parse NUL-separated output: gstatus\0prefix\0subject
  gstatus=${info%%$'\x00'*}
  info=${info#*$'\x00'}
  prefix=${info%%$'\x00'*}
  subject=${info#*$'\x00'}

  # Branch from "# branch.head <name>" line in porcelain v2 output
  branch=${gstatus#*branch.head }
  branch=${branch%%$'\n'*}
  [[ -z "$branch" || "$branch" == "# "* ]] && return

  # Dirty if any non-header line exists (file status lines don't start with #)
  [[ $gstatus == *$'\n'[^#]* ]] && dirty='*'

  # At root if prefix is empty (rev-parse --show-prefix returns empty at root)
  [[ "$prefix" == $'\n' || -z "$prefix" ]] && root='~'

  # PAUSED badge for git-pause workflow (commit with "PAUSED: ..." message)
  [[ $subject == PAUSED* ]] && paused=' %B%F{198}%K{52}[PAUSED]%b%f%k'

  printf '%s%%F{yellow} (%s%s%s)%%f ' "$paused" "$branch" "$dirty" "$root"
}

# Disable the default virtualenv prompt modification. Python's venv activation
# script prepends "(venv)" to PS1, but we handle this ourselves in __is_venv
# for consistent styling.
VIRTUAL_ENV_DISABLE_PROMPT=1

setopt PROMPT_SUBST
autoload -U colors && colors

# Display a custom tag in the prompt. Useful for labeling terminal sessions
# (e.g., "api", "frontend", "debug"). Set via `tag "label"`, clear with `tag`.
__tag() {
  [[ -n "$_PROMPT_TAG" ]] && echo "%B%F{87}%K{20}[${(U)_PROMPT_TAG}]%b%f%k "
}

tag() { _PROMPT_TAG="$1" }

PROMPT='%(?..%F{red}?%? )$(__tag)$(__is_venv)%f%3~%f$(__git_prompt_segment)%# '

# Auto-start tmux for interactive terminal sessions, unless inside Emacs
# (which has its own window management) or Hammerspoon (macOS automation).
# Conditions: stdin is a tty, not already in tmux, shell is interactive.
if (( ! ${+INSIDE_EMACS} && ! ${+INSIDE_HS} )); then
  if [[ -t 0 && -z "$TMUX" && $- == *i* ]]; then
    exec tmux
  fi
fi
