# smallwat3r's config for zsh

export LANGUAGE=en_US.UTF-8
export LANG=en_US.UTF-8
export LC_CTYPE=en_US.UTF-8
export LC_ALL=en_US.UTF-8
export LDFLAGS='-L/usr/local/opt/python@3.8/lib'
export PER5LIB="$HOME/lib/perl5"
export TERM='xterm-256color'
export CLICOLOR=1
export GREP_OPTIONS='--color=auto'
export GREP_COLOR='0;30;42'
export HISTFILE="$HOME/.zsh_history"
export HISTSIZE=999999999 # store (almost) infinite history
export SAVEHIST=$HISTSIZE
export HOMEBREW_NO_ANALYTICS=1
export HOMEBREW_NO_AUTO_UPDATE=1

setopt AUTO_CD              # auto cd into typed directory
setopt CHASE_LINKS          # resolve symlinks to their true values when changing directory
setopt GLOB_DOTS            # do not require a leading ‘.’ in a filename to be matched explicitly
setopt INTERACTIVE_COMMENTS # allow comments in interactive shell
setopt LIST_PACKED          # make the completion list occupying less lines
setopt VI                   # Emulate vim mode in zsh (same as 'bindkey -v')
setopt APPEND_HISTORY       # keep history of commands
setopt EXTENDED_HISTORY     # add timestamp and duration to the history
setopt INC_APPEND_HISTORY   # add commands as soon as they are entered
setopt HIST_REDUCE_BLANKS   # get rid of superfluous blank lines
setopt HIST_VERIFY          # perform history expansion and reload the line into the editing buffer.
unsetopt BEEP               # do no beep on errors
unsetopt LIST_BEEP          # do not beep on anbiguous completion

fpath=($HOME/.zsh/functions $fpath)
autoload -U "$HOME"/.zsh/functions/*(:t)

[[ -f "$HOME/.zsh/.aliases" ]] && source "$HOME/.zsh/.aliases"

# Adding more support to zsh vim mode
export KEYTIMEOUT=20 # if the keytimeout was too short, jk wouldn't work for ESC
bindkey -M viins 'jk' vi-cmd-mode
bindkey '^?' backward-delete-char

# yank to clipboard
_vi_yank_pbcopy() {
  zle vi-yank
  echo "$CUTBUFFER" | pbcopy
}
zle -N _vi_yank_pbcopy
bindkey -M vicmd 'y' _vi_yank_pbcopy

# Edit command in preferred editor
autoload edit-command-line
zle -N edit-command-line
bindkey '^e' edit-command-line

# Move up directories i.e. ... >> ../.. and .... >> ../../... etc.
__rationalise-dot() {
  [[ $LBUFFER = *.. ]] && LBUFFER+=/.. || LBUFFER+=.
}
zle -N __rationalise-dot
bindkey "." __rationalise-dot

autoload -U compinit && compinit
zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path "$HOME/.cache/.zcompcache"
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*' verbose yes
zstyle ':completion:*' file-sort modification

zmodload zsh/complist
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'j' vi-down-line-or-history
bindkey -M menuselect 'l' vi-forward-char

VIRTUAL_ENV_DISABLE_PROMPT=false
setopt PROMPT_SUBST

__git_root() {
  if [[ $(git rev-parse --is-inside-work-tree 2>/dev/null) == true ]]; then
    [[ $(git rev-parse --show-toplevel 2>/dev/null) == "$PWD" ]] && echo true
  fi
}

__git_dirty() {
  [[ $(git diff --shortstat 2>/dev/null | tail -n1) != "" ]] && echo "*"
}

__git_branch() {
  git branch --no-color 2>/dev/null \
    | sed -e '/^[^*]/d' -e "s/* \(.*\)/\1$(__git_dirty)/"
}

__display_git_info() {
  local _git_root
  local _git_branch
  _git_root="$(__git_root | sed 's/true/~/')"
  _git_branch="$(__git_branch)"
  [[ -n $_git_branch ]] && echo " ${_git_branch}${_git_root}"
}

__shrink_path() {
  echo ~+ \
    | sed "s;$HOME;~;" \
    | sed 's;\(/.\)[^/]*;\1;g' \
    | sed 's/.$//'
}

__path() {
  case $PWD in
    "$HOME") printf '~' ;;
    *) printf '%s%s' "$(__shrink_path)" "${PWD##*/}" ;;
  esac
}

__is_venv() {
  if [[ $VIRTUAL_ENV ]]; then
    echo '%s' "(.${VIRTUAL_ENV##*/}) "
  fi
}

export VIRTUAL_ENV_DISABLE_PROMPT=false
setopt PROMPT_SUBST

# If we are from Emacs, run a standard prompt and do not run tmux. Else, in other shell emulators
# activate tmux by default and use the individual pane titles to display the main prompt information.
if [[ "$INSIDE_EMACS" ]]; then
  PROMPT='%(?..%? )$(__is_venv)$(__path)$(__display_git_info) %# '
else
  if [ -t 0 ] && [[ -z $TMUX ]] && [[ $- = *i* ]]; then
    exec tmux
  fi

  PROMPT='%(?..%? )$(__is_venv)%# '

  __pane_number() {
    tmux list-panes | grep "active" | cut -d ':' -f 1
  }

  ssh() {
    [[ -z $TMUX ]] \
      || tmux select-pane \
        -t "$(__pane_number)" \
        -T "#[fg=red,bold]$(echo "$*" | cut -d . -f 1)#[fg=default]"
    command ssh "$@"
  }

  precmd() {
    [[ -z $TMUX ]] \
      || tmux select-pane \
        -t "$(__pane_number)" \
        -T "$(__path)$(__display_git_info)"
  }
fi

[[ -f "$HOME/.kubeprivate" ]] && source "$HOME/.kubeprivate"
[[ -f "$EMACS_DOOM/.local/straight/repos/emacs-libvterm/etc/emacs-vterm-zsh.sh" ]] \
  && source "$EMACS_DOOM/.local/straight/repos/emacs-libvterm/etc/emacs-vterm-zsh.sh"
