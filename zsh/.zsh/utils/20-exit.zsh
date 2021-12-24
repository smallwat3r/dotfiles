# By default, Ctrl+d will not close your shell if the command line
# is filled, this fixes it.

__exit_zsh() {
  exit
}

zle -N __exit_zsh
bindkey '^D' __exit_zsh
