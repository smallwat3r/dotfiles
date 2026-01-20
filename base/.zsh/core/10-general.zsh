# General configurations

unsetopt BEEP               # do no beep on errors
unsetopt LIST_BEEP          # do not beep on anbiguous completion
unsetopt COMPLETE_ALIASES   # do not prevent aliases from being substituted before completion is attempted

setopt AUTO_CD              # auto cd into typed directory
setopt CHASE_LINKS          # resolve symlinks to their true values when changing directory
setopt GLOB_DOTS            # do not require a leading '.' in a filename to be matched explicitly
setopt INTERACTIVE_COMMENTS # allow comments in interactive shell
setopt LIST_PACKED          # make the completion list occupying less lines

# history configs
setopt APPEND_HISTORY       # keep history of commands
setopt EXTENDED_HISTORY     # add timestamp and duration to the history
setopt SHARE_HISTORY        # share history across all sessions in real-time
setopt HIST_REDUCE_BLANKS   # get rid of superfluous blank lines
setopt HIST_VERIFY          # perform history expansion and reload the line into the editing buffer
setopt HISTIGNORESPACE      # do not save in history commands that starts by a space
export HISTFILE="${HOME}/.zsh_history"
export HISTSIZE=1000000
export SAVEHIST="${HISTSIZE}"

# ensure keys are mapped correctly
bindkey "^[[1;5C" forward-word
bindkey "^[[1;5D" backward-word

# ctrl + backspace to delete whole word
if [[ "$OSTYPE" == darwin* ]]; then
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
    # already quoted -> remove
    local inner=${word[2,-2]}
    BUFFER="${prefix}${inner}${suffix}"
    CURSOR=$(( ${#prefix} + ${#inner} ))
  else
    # unquoted -> add quotes
    BUFFER="${prefix}\"${word}\"${suffix}"
    CURSOR=$(( ${#prefix} + ${#word} + 2 ))
  fi
}
zle -N __toggle_quotes
bindkey '^O' __toggle_quotes
