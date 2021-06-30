# ZSH config
# ~~~~~~~~~~

# Source my functions and aliases
[[ -f "$HOME/.aliases" ]] && source "$HOME/.aliases" end
[[ -f "$HOME/.functions" ]] && source "$HOME/.functions" end

# vterm
[[ -f "$HOME/.emacs.d/.local/straight/repos/emacs-libvterm/etc/emacs-vterm-zsh.sh" ]] &&
  source "$HOME/.emacs.d/.local/straight/repos/emacs-libvterm/etc/emacs-vterm-zsh.sh" end

# fzf
[[ -f "$HOME/.fzf.zsh" ]] && source "$HOME/.fzf.zsh" end

# Private kube functions
[[ -f "$HOME/.kubeprivate" ]] && source "$HOME/.kubeprivate" end


#
## General settings


export TERM='xterm-256color'

export LANGUAGE=en_US.UTF-8
export LANG=en_US.UTF-8
export LC_CTYPE=en_US.UTF-8
export LC_ALL=en_US.UTF-8

export GREP_OPTIONS='--color=auto'
export GREP_COLOR='0;30;42'

export CLICOLOR=1

export LDFLAGS='-L/usr/local/opt/python@3.8/lib'
export PER5LIB="$HOME/lib/perl5"

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

# Search history
autoload -Uz up-line-or-beginning-search down-line-or-beginning-search

zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search

# TODO: Fix these
bindkey '^[[A' up-line-or-beginning-search # Arrow up
bindkey '^[OA' up-line-or-beginning-search
bindkey '^[[B' down-line-or-beginning-search # Arrow down
bindkey '^[OB' down-line-or-beginning-search

bindkey -M vicmd 'k' up-line-or-beginning-search
bindkey -M vicmd 'j' down-line-or-beginning-search

# If the keytimeout was too short, jk wouldn't work for ESC
export KEYTIMEOUT=20

# Emulate vim mode in zsh
bindkey -v
bindkey -M viins 'jk' vi-cmd-mode
bindkey '^?' backward-delete-char

# yank to clipboard
_vi_yank_pbcopy() {
  zle vi-yank
  echo "$CUTBUFFER" | pbcopy
}
zle -N _vi_yank_pbcopy
bindkey -M vicmd 'y' _vi_yank_pbcopy

# edit command in preferred editor
autoload edit-command-line
zle -N edit-command-line
bindkey '^e' edit-command-line


#
## Plugins


# homebrew
export HOMEBREW_NO_ANALYTICS=1
export HOMEBREW_NO_AUTO_UPDATE=1

# ripgrep
export RIPGREP_CONFIG_PATH="$HOME/.config/.ripgreprc"

export FZF_DEFAULT_OPTS='
  --height 96% --reverse --border
  --color dark,hl:202,hl+:202,bg+:#101010,fg+:10
  --color info:10,prompt:202,spinner:10,pointer:10,marker:10
'
export FZF_DEFAULT_COMMAND='rg --files --hidden --follow -g "!__pycache__/" -g "!.git/"'

# antigen (load external plugins)
[[ -f '/usr/local/share/antigen/antigen.zsh' ]] && {
  source '/usr/local/share/antigen/antigen.zsh'

  antigen bundle zsh-users/zsh-syntax-highlighting
  antigen bundle zsh-users/zsh-autosuggestions
  antigen bundle hlissner/zsh-autopair
  antigen bundle skywind3000/z.lua
  antigen bundle johanhaleby/kubetail

  antigen apply
}

# zsh syntax-highlight options

# By default only main is activated
export ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern cursor)

# Custom colors
typeset -A ZSH_HIGHLIGHT_STYLES
export ZSH_HIGHLIGHT_STYLES[command]='fg=green,bold'
export ZSH_HIGHLIGHT_STYLES[builtin]='fg=green,bold'
export ZSH_HIGHLIGHT_STYLES[function]='fg=cyan,bold'
export ZSH_HIGHLIGHT_STYLES[alias]='fg=cyan,bold'
export ZSH_HIGHLIGHT_STYLES[globbing]='fg=magenta,bold'
export ZSH_HIGHLIGHT_STYLES[redirection]='fg=magenta,bold'

# zsh auto-suggestions options
export ZSH_AUTOSUGGEST_USE_ASYNC=true
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=241'
export ZSH_AUTOSUGGEST_STRATEGY=(history completion)


#
## Prompt stuff


export VIRTUAL_ENV_DISABLE_PROMPT=false
setopt PROMPT_SUBST

_display_git_info() {
  local _git_root=$(echo $(git-root) | sed 's/true/~/')
  local _git_branch=$(echo $(git-branch))
  [[ ! -z $_git_branch ]] && echo " ${_git_branch}${_git_root}"
}

if [[ "$INSIDE_EMACS" ]]; then
  # If we are in Emacs, run a standard prompt and do not run tmux.
  PROMPT='%(?..%{$fg[red]%}%? )$resetcolor$(is-venv)$(shpwd)$(_display_git_info) %# '
else
  # In other shell emulators: activate tmux by default
  # As tmux is activated by default, its using the individual pane titles
  # to display the prompt information.

  # Auto-activate tmux
  if [ -t 0 ] && [[ -z $TMUX ]] && [[ $- = *i* ]]; then
    exec tmux
  fi

  PROMPT='%(?..%{$fg[red]%}%? )$resetcolor$(is-venv)%# '

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
fi


#
## Completion


autoload -Uz compinit && compinit

zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*' accept-exact '*(N)'
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path "$HOME/.cache"
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*' verbose true
zstyle ':completion:*:rm:*' ignore-line-yes
zstyle ':completion:*:cd:*' ignore-parents parent pwd
zstyle ':completion:*:options' list-colors '=^(-- *)=34'


#
## Misc


# Move up directories (... automatically becomes ../..)
_rationalise-dot() {
  if [[ $LBUFFER = *.. ]]; then
    LBUFFER+=/..
  else
    LBUFFER+=.
  fi
}
zle -N _rationalise-dot
bindkey "." _rationalise-dot
