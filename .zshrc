# ZSH config.
# Matthieu Petiteau <mpetiteau.pro@gmail.com>

# {{{1 env

# {{{2 miscellaneous

export TERM="xterm-256color"
export CLICOLOR=1
export EDITOR="/usr/local/bin/nvim"
export LDFLAGS="-L/usr/local/opt/python@3.8/lib"
export LANG="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"
export KEYTIMEOUT=1

# }}}2
# {{{2 paths

export PATH="/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin"
export PATH="$HOME/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="/usr/local/opt/python@3.8/bin:$PATH"
export GOPATH="$HOME/go"
export PATH="$HOME/go/bin:$PATH"

# }}}2
# {{{2 grep

export GREP_OPTIONS='--color=auto'
export GREP_COLOR='0;30;42'

# }}}2
# {{{2 homebrew

export HOMEBREW_NO_ANALYTICS=1
export HOMEBREW_NO_AUTO_UPDATE=1

# }}}2
# {{{2 history

export HISTFILE=~/.zsh_history
export HISTSIZE=999999999
export SAVEHIST=$HISTSIZE

# }}}2

# }}}1 env
# {{{1 source

source $HOME/.aliases
source $HOME/.functions
source /usr/local/share/antigen/antigen.zsh
[[ -f $HOME/.fzf.zsh ]] && source $HOME/.fzf.zsh

# }}}1 source
# {{{1 antigen

antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-autosuggestions

antigen apply

# }}}1 antigen
# {{{1 general

# zsh auto-suggestions colors
ZSH_AUTOSUGGEST_USE_ASYNC=true
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=200"
ZSH_AUTOSUGGEST_STRATEGY=(history completion)

# set options
setopt print_exit_value
setopt correct
setopt chase_links
setopt auto_remove_slash
setopt glob_dots

# history options
setopt append_history
setopt share_history
setopt inc_append_history
setopt hist_reduce_blanks
setopt hist_verify
setopt hist_ignore_all_dups

# unset options
unsetopt beep
unsetopt list_beep
unsetopt ignore_eof

# starship prompt
eval "$(starship init zsh)"

# fzf
export FZF_DEFAULT_OPTS="
--height 96% --reverse --border
--color fg:231,bg:0,hl:199,fg+:231,bg+:0,hl+:190
--color info:189,prompt:161,spinner:201,pointer:201,marker:118
"
export FZF_DEFAULT_COMMAND='rg --files --hidden --follow -g "!__pycache__/" -g "!.git/"'

# launch tmux on start
case $- in *i*)
  [[ -z $TMUX ]] && exec tmux
esac

# }}}1 general
# {{{1 vim mode

# activate vim-mode
bindkey -v

# yank to clipboard
vi_yank_pbcopy() {
  zle vi-yank
  echo "$CUTBUFFER" |
    pbcopy
}
zle -N vi_yank_pbcopy
bindkey -M vicmd 'y' vi_yank_pbcopy

bindkey '^?' backward-delete-char
bindkey '^h' backward-delete-char
bindkey '^w' backward-kill-word

# }}}1 vim mode
# {{{1 completion

zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*' accept-exact '*(N)'
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.cache
zstyle ':completion:*:rm:*' ignore-line-yes

# }}}1 completion
