#!/bin/zsh
# File  : prompt.zsh
# Author: Matthieu Petiteau <mpetiteau.pro@gmail.com>
# Date  : 20.12.2019
#
# ZSH Prompt
#

local return_code="%(?..%{$fg[red]%}%? ↵%{$reset_color%})"
PROMPT='%{$fg_bold[white]%}$(git_prompt_info)%20<...<%~%<< %{$reset_color%}%% '
RPS1="${return_code}"
