# ZSH config.
# Matthieu Petiteau <mpetiteau.pro@gmail.com>

# {{{1 env

export PATH="/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin"
export PATH="$HOME/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="/usr/local/opt/python@3.8/bin:$PATH"
export GOPATH="$HOME/go"
export PATH="$HOME/go/bin:$PATH"

export TERM="xterm-256color"
export CLICOLOR=1
export EDITOR="/usr/local/bin/nvim"
export LDFLAGS="-L/usr/local/opt/python@3.8/lib"
export LANG="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"
export PER5LIB="$HOME/lib/perl5"

# }}}1 env
# {{{1 source

[[ -f "$HOME/.aliases" ]] && source "$HOME/.aliases"
[[ -f "$HOME/.functions" ]] && source "$HOME/.functions"
[[ -f "/usr/local/share/antigen/antigen.zsh" ]] && source "/usr/local/share/antigen/antigen.zsh"
[[ -f "$HOME/.fzf.zsh" ]] && source "$HOME/.fzf.zsh"

# }}}1 source
# {{{1 antigen

antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-autosuggestions
antigen bundle hlissner/zsh-autopair

antigen apply

# }}}1 antigen
# {{{1 general

# zsh auto-suggestions colors
export ZSH_AUTOSUGGEST_USE_ASYNC=true
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=37"
export ZSH_AUTOSUGGEST_STRATEGY=(history completion)

# colors
autoload -U colors && colors

# set options
setopt AUTOCD
setopt PRINT_EXIT_VALUE
setopt CHASE_LINKS
setopt AUTO_REMOVE_SLASH
setopt GLOB_DOTS

# history options
setopt APPEND_HISTORY
setopt EXTENDED_HISTORY
setopt INC_APPEND_HISTORY
setopt HIST_REDUCE_BLANKS
setopt HIST_VERIFY
setopt HIST_IGNORE_ALL_DUPS

export HISTFILE="$HOME/.zsh_history"
export HISTSIZE=999999999
export SAVEHIST=$HISTSIZE

# homebrew
export HOMEBREW_NO_ANALYTICS=1
export HOMEBREW_NO_AUTO_UPDATE=1

# grep / ripgrep
export GREP_OPTIONS='--color=auto'
export GREP_COLOR='0;30;42'
export RIPGREP_CONFIG_PATH="$HOME/.ripgreprc"

# unset options
unsetopt BEEP
unsetopt LIST_BEEP
unsetopt IGNORE_EOF

# fzf
export FZF_DEFAULT_OPTS='
  --height 96% --reverse --border
  --color dark,hl:37,hl+:37,bg+:#002b36,fg+:136
  --color info:136,prompt:37,spinner:136,pointer:230,marker:230
'
export FZF_DEFAULT_COMMAND='rg --files --hidden --follow -g "!__pycache__/" -g "!.git/"'

# }}}1 general
# {{{1 prompt

# launch tmux on start
case $- in *i*)
  [[ -z $TMUX ]] && exec tmux
esac

setopt PROMPT_SUBST

# venv
export VIRTUAL_ENV_DISABLE_PROMPT=false
_is_venv() {
  [[ $VIRTUAL_ENV ]] && echo "(.${VIRTUAL_ENV##*/}) "
}

# zsh vim mode
vim_ins_mode='%#'
vim_cmd_mode=';;'
vim_mode=$vim_ins_mode

function zle-keymap-select {
  vim_mode="${${KEYMAP/vicmd/${vim_cmd_mode}}/(main|viins)/${vim_ins_mode}}"
  zle reset-prompt
}
zle -N zle-keymap-select

function zle-line-finish {
  vim_mode=$vim_ins_mode
}
zle -N zle-line-finish

# actual prompt
PROMPT='$(_is_venv)${vim_mode} '

# Use tmux pane title as prompt.
precmd() {
  local _cur_pane=$(
    tmux list-panes |
      grep "active" |
      cut -d ':' -f 1
  )
  tmux select-pane -t $_cur_pane -T "$(/usr/local/bin/shpwd) $(/usr/local/bin/git_branch)"
}

# }}}1 prompt
# {{{1 completion

autoload -U compinit

zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*' accept-exact '*(N)'
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.cache
zstyle ':completion:*:rm:*' ignore-line-yes

zmodload zsh/complist
compinit

# }}}1 completion
# {{{1 vim mode

# activate vim-mode
bindkey -v
export KEYTIMEOUT=1

# yank to clipboard
_vi_yank_pbcopy() {
  zle vi-yank
  echo "$CUTBUFFER" |
    pbcopy
}
zle -N _vi_yank_pbcopy
bindkey -M vicmd 'y' _vi_yank_pbcopy

# mappings
bindkey '^?' backward-delete-char
bindkey '^h' backward-delete-char
bindkey '^w' backward-kill-word

# vim keys in tab complete menu
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history

# edit command in vim
autoload edit-command-line
zle -N edit-command-line
bindkey '^e' edit-command-line

# }}}1 vim mode
