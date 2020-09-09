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
