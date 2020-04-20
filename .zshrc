# ZSH config.
# Matthieu Petiteau <mpetiteau.pro@gmail.com>

# {{{1 env

# {{{2 miscellaneous

export ZSH="$HOME/.oh-my-zsh"
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
[[ -f ~/.fzf.zsh ]] && source $HOME/.fzf.zsh

# }}}1 source
# {{{1 antigen

antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-autosuggestions
antigen bundle softmoth/zsh-vim-mode

antigen apply

# }}}1 antigen
# {{{1 completion

zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache
zstyle ':completion::complete:*' use-cache 1
zstyle ':completion:*:rm:*' ignore-line-yes

# }}}1 completion
# {{{1 prompt

# zsh auto-suggestions colors
ZSH_AUTOSUGGEST_USE_ASYNC=true
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=200"
ZSH_AUTOSUGGEST_STRATEGY=(history completion)

# starship prompt
eval "$(starship init zsh)"

# }}}1 prompt
# {{{1 misc

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

# zsh vim yank to clipboard (using pbcopy)
vi_yank_pbcopy() {
  zle vi-yank
  echo "$CUTBUFFER" |
    pbcopy
}
zle -N vi_yank_pbcopy
bindkey -M vicmd 'y' vi_yank_pbcopy

# }}}1 misc
