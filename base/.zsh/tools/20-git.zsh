# Git related configurations

has git || return

export GIT_SSH_COMMAND='ssh -4 \
  -o ConnectTimeout=10 \
  -o ServerAliveInterval=20 \
  -o ServerAliveCountMax=3 \
  -o TCPKeepAlive=yes \
  -o GSSAPIAuthentication=no \
  -o ControlMaster=no'

# Create short `g<alias>` versions of all git aliases.
() {
  local line key name
  local git_alias_lines

  git_alias_lines=("${(@f)$(git config --get-regexp '^alias\.' 2>/dev/null)}")

  for line in $git_alias_lines; do
    key=${line%% *}       # "alias.co"
    name=${key#alias.}    # "co"
    alias "g${name}=git ${name}"
  done

  alias g="git"
}
