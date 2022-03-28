# Edit current command in default terminal editor

autoload -U edit-command-line

zle -N edit-command-line
bindkey '^e' edit-command-line
