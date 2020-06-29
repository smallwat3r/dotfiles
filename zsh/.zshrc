# ZSH config.

# {{{1 general

# launch tmux by default
if [ -t 0 ] && [[ -z $TMUX ]] && [[ $- = *i* ]]; then
  exec tmux
fi

export PATH='/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin'
export PATH="$HOME/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="/usr/local/opt/python@3.8/bin:$PATH"
export GOPATH="$HOME/go"
export PATH="$HOME/go/bin:$PATH"

export TERM='xterm-256color'
export CLICOLOR=1
export EDITOR='/usr/local/bin/nvim'
export LDFLAGS='-L/usr/local/opt/python@3.8/lib'
export LANG='en_US.UTF-8'
export LC_ALL='en_US.UTF-8'
export PER5LIB="$HOME/lib/perl5"

# functions and aliases
[[ -f "$HOME/.aliases" ]] && source "$HOME/.aliases"
[[ -f "$HOME/.functions" ]] && source "$HOME/.functions"

# zsh auto-suggestions colors
export ZSH_AUTOSUGGEST_USE_ASYNC=true
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=37'
export ZSH_AUTOSUGGEST_STRATEGY=(history completion)

# colors
autoload -U colors && colors

# zsh options
setopt AUTOCD
setopt CHASE_LINKS
setopt AUTO_REMOVE_SLASH
setopt GLOB_DOTS
setopt INTERACTIVE_COMMENTS

unsetopt BEEP
unsetopt LIST_BEEP
unsetopt IGNORE_EOF

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
export RIPGREP_CONFIG_PATH="$HOME/.config/.ripgreprc"

# fzf
[[ -f "$HOME/.fzf.zsh" ]] && source "$HOME/.fzf.zsh"

export FZF_DEFAULT_OPTS='
  --height 96% --reverse --border
  --color dark,hl:37,hl+:37,bg+:#101010,fg+:136
  --color info:136,prompt:37,spinner:136,pointer:230,marker:230
'
export FZF_DEFAULT_COMMAND='rg --files --hidden --follow -g "!__pycache__/" -g "!.git/"'

# }}}1 general
# {{{1 antigen

[[ -f '/usr/local/share/antigen/antigen.zsh' ]] && source '/usr/local/share/antigen/antigen.zsh'

antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-autosuggestions
antigen bundle hlissner/zsh-autopair

antigen apply

# }}}1 antigen
# {{{1 prompt

setopt PROMPT_SUBST

# zsh vim mode
vim_ins_mode='%#'
vim_cmd_mode=';;'
vim_mode=$vim_ins_mode

zle-keymap-select() {
  vim_mode="${${KEYMAP/vicmd/${vim_cmd_mode}}/(main|viins)/${vim_ins_mode}}"
  zle reset-prompt
}
zle -N zle-keymap-select

zle-line-finish() {
  vim_mode=$vim_ins_mode
}
zle -N zle-line-finish

# Tmux is turn on by default, so I'm using the individual pane titles
# to display the majority of the prompt information.

export VIRTUAL_ENV_DISABLE_PROMPT=false
PROMPT='%(?..%{$fg[red]%}%? )$resetcolor$(is_venv)${vim_mode} '

_display_git_info() {
  local _git_root=$(echo $(git_root) | sed 's/true/~/')
  local _git_branch=$(echo $(git_branch))
  [[ ! -z $_git_branch ]] && echo " $_git_branch$_git_root"
}

_pane_number() {
  echo $(tmux list-panes | grep "active" | cut -d ':' -f 1)
}

ssh() {
  [[ -z $TMUX ]] ||
    tmux select-pane -t $(_pane_number) -T "#[fg=red,bold]$(echo $* | cut -d . -f 1)#[fg=default]"
  command ssh "$@"
}

precmd() {
  [[ -z $TMUX ]] ||
    tmux select-pane -t $(_pane_number) -T "$(shpwd)$(_display_git_info)"
}

# }}}1 prompt
# {{{1 completion

autoload -U compinit

zstyle ':completion:*'      list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*'      menu select
zstyle ':completion:*'      matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*'      accept-exact '*(N)'
zstyle ':completion:*'      use-cache on
zstyle ':completion:*'      cache-path ~/.cache
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
  echo "$CUTBUFFER" | pbcopy
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
