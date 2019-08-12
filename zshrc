export ZSH="/Users/smallwat3r/.oh-my-zsh"

PROMPT='%{$fg_bold[red]%}$(git_prompt_info)%{$reset_color%}$ '

ZSH_THEME_GIT_PROMPT_PREFIX="("
ZSH_THEME_GIT_PROMPT_SUFFIX=")"
ZSH_THEME_GIT_PROMPT_DIRTY="ko"
ZSH_THEME_GIT_PROMPT_CLEAN="ok"

# DISABLE_LS_COLORS="true"
DISABLE_AUTO_TITLE="true"
COMPLETION_WAITING_DOTS="true"

DISABLE_UNTRACKED_FILES_DIRTY="true"

plugins=(git)

source $ZSH/oh-my-zsh.sh

export EDITOR='vim'

alias p="pwd"
alias s="sketch"
alias n="nnn"

source /sw/bin/init.sh
fpath+=${ZDOTDIR:-~}/.zsh_functions

if command -v tmux>/dev/null; then
  [[ ! $TERM =~ screen ]] && [ -z $TMUX ] && tmux
fi
