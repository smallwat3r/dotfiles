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

# ctrl + d to exit shell
__exit_zsh() { exit }
zle -N __exit_zsh
bindkey '^D' __exit_zsh

# up and down to go through history
autoload -U history-search
bindkey "^[[A" history-beginning-search-backward
bindkey "^[[B" history-beginning-search-forward
