# Provides bindings to go through commands history

autoload -U history-search

bindkey "^[[A" history-beginning-search-backward
bindkey "^[[B" history-beginning-search-forward
