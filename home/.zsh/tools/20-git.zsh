# Git configuration
#
# Auto-generates g<alias> shortcuts from git aliases (e.g., gco for
# git checkout). SSH reliability settings live in ~/.ssh/config.

has git || return

alias g="git"

# Create short `g<alias>` versions of all git aliases.
local __git_line __git_name
for __git_line in "${(@f)$(git config --get-regexp '^alias\.' 2>/dev/null)}"; do
  __git_name=${${__git_line%% *}#alias.}
  alias "g${__git_name}=git ${__git_name}"
done
unset __git_line __git_name
