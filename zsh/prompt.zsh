#!/bin/zsh
# File  : prompt.zsh
# Author: Matthieu Petiteau <mpetiteau.pro@gmail.com>
# Date  : 20.12.2019
#
# ZSH Prompt
#

parse_git_branch() {
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}

setopt PROMPT_SUBST

local return_code="%(?..%{$fg[red]%}%? ↵%{$reset_color%})"
PROMPT='%{$fg_bold[white]%}$(parse_git_branch) %20<...<%~%<< %{$reset_color%}%% '
RPS1="${return_code}"
