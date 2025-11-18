# history configs
export HISTFILE="${HOME}/.zsh_history"
export HISTSIZE=999999999
export SAVEHIST="${HISTSIZE}"

# ensure keys are mapped correctly
bindkey "^[[1;5C" forward-word
bindkey "^[[1;5D" backward-word

# ctrl + backspace to delete whole word
if [[ "$(uname -s)" == "Darwin" ]]; then
  bindkey "^?" backward-kill-word
else
  bindkey "^H" backward-kill-word
fi

# ctrl + e to edit command line
autoload -U edit-command-line
zle -N edit-command-line
bindkey '^e' edit-command-line

# ctrl + d to exit shells
__exit_zsh() { exit }
zle -N __exit_zsh
bindkey '^D' __exit_zsh

# up and down to go through history
autoload -U history-search
bindkey "^[[A" history-beginning-search-backward
bindkey "^[[B" history-beginning-search-forward

# toggle quotes around a word
__toggle_quotes() {
  emulate -L zsh
  setopt localoptions extendedglob

  local l=$LBUFFER
  local r=$RBUFFER

  # find start of the word under cursor
  local i=$CURSOR
  while (( i > 0 )) && [[ ${l[i]} != " " && ${l[i]} != $'\t' ]]; do
    (( i-- ))
  done
  local start=$(( i + 1 ))

  # find end of the word under cursor
  local len=${#BUFFER}
  local j=$(( CURSOR + 1 ))
  while (( j <= len )) && [[ ${BUFFER[j]} != " " && ${BUFFER[j]} != $'\t' ]]; do
    (( j++ ))
  done
  local end=$(( j - 1 ))

  # if no word found, do nothing
  (( end < start )) && return

  # extract word + prefix + suffix
  local prefix=${BUFFER[1,start-1]}
  local word=${BUFFER[start,end]}
  local suffix=${BUFFER[end+1,-1]}

  if [[ $word == \"*\" && $word == *\" ]]; then
    # already quoted —> remove
    local inner=${word[2,-2]}
    BUFFER="${prefix}${inner}${suffix}"
    CURSOR=$(( ${#prefix} + ${#inner} ))
  else
    # unquoted —> add quotes
    BUFFER="${prefix}\"${word}\"${suffix}"
    CURSOR=$(( ${#prefix} + ${#word} + 2 ))
  fi
}
zle -N __toggle_quotes
bindkey '^O' __toggle_quotes
