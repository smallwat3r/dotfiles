# Git configuration
#
# Auto-generates g<alias> shortcuts from git aliases (e.g., gco for
# git checkout). SSH reliability settings live in ~/.ssh/config.

has git || return

alias g="git"

# Create short `g<alias>` versions of all git aliases, read straight from
# the [alias] section of the config file so no git process is forked.
() {
  local file=${XDG_CONFIG_HOME:-$HOME/.config}/git/config line name in_alias=0
  [[ -r $file ]] || return 0
  for line in "${(@f)$(<$file)}"; do
    if [[ $line == '['* ]]; then
      [[ $line == '[alias]' ]] && in_alias=1 || in_alias=0
      continue
    fi
    (( in_alias )) || continue
    name=${${line%%=*}//[[:space:]]/}
    [[ -z $name || $name == '#'* ]] && continue
    alias "g${name}=git ${name}"
  done
}
