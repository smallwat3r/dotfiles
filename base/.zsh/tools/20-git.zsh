# Git configuration
#
# Auto-generates g<alias> shortcuts from git aliases (e.g., gco for
# git checkout). SSH reliability settings live in ~/.ssh/config.

has git || return

# Create short `g<alias>` versions of all git aliases.
# Deferred to first prompt so the git fork doesn't block startup.
__load_git_aliases() {
  local line key name
  local git_alias_lines

  git_alias_lines=(
    "${(@f)$(git config --get-regexp '^alias\.' 2>/dev/null)}"
  )

  for line in $git_alias_lines; do
    key=${line%% *}       # "alias.co"
    name=${key#alias.}    # "co"
    alias "g${name}=git ${name}"
  done

  alias g="git"
  add-zsh-hook -d precmd __load_git_aliases
  unfunction __load_git_aliases
}

autoload -Uz add-zsh-hook
add-zsh-hook precmd __load_git_aliases
