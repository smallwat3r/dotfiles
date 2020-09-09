# Zsh settings
# ~~~~~~~~~~~~

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

# grep / ripgrep
export GREP_OPTIONS='--color=auto'
export GREP_COLOR='0;30;42'

# Search history
# --------------
autoload -Uz up-line-or-beginning-search down-line-or-beginning-search

zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search

bindkey '^[[A' up-line-or-beginning-search    # Arrow up
bindkey '^[OA' up-line-or-beginning-search
bindkey '^[[B' down-line-or-beginning-search  # Arrow down
bindkey '^[OB' down-line-or-beginning-search

# Activate vim-mode
# -----------------
bindkey -v
export KEYTIMEOUT=1

# yank to clipboard
_vi_yank_pbcopy() {
  zle vi-yank
  echo "$CUTBUFFER" | pbcopy
}
zle -N _vi_yank_pbcopy
bindkey -M vicmd 'y' _vi_yank_pbcopy

# edit command in vim
autoload edit-command-line
zle -N edit-command-line
bindkey '^e' edit-command-line
