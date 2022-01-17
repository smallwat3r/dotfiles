# smallwat3r's config for zsh

zmodload zsh/zprof  # profiler

setopt AUTO_CD              # auto cd into typed directory
setopt CHASE_LINKS          # resolve symlinks to their true values when changing directory
setopt GLOB_DOTS            # do not require a leading '.' in a filename to be matched explicitly
setopt INTERACTIVE_COMMENTS # allow comments in interactive shell
setopt LIST_PACKED          # make the completion list occupying less lines
setopt APPEND_HISTORY       # keep history of commands
setopt EXTENDED_HISTORY     # add timestamp and duration to the history
setopt INC_APPEND_HISTORY   # add commands as soon as they are entered
setopt HIST_REDUCE_BLANKS   # get rid of superfluous blank lines
setopt HIST_VERIFY          # perform history expansion and reload the line into the editing buffer.
setopt HISTIGNORESPACE      # do not save in history commands that starts by a space

unsetopt BEEP               # do no beep on errors
unsetopt LIST_BEEP          # do not beep on anbiguous completion

ZSH_ROOT="${HOME}/.zsh"

# Load functions
fpath=("${ZSH_ROOT}"/functions $fpath)
autoload -U "${ZSH_ROOT}"/functions/*(:t)

# Load config files
zsh_configs=(
  "${ZSH_ROOT}"/prompt.zsh
  "${ZSH_ROOT}"/aliases
  "${ZSH_ROOT}"/utils
  "${ZSH_ROOT}"/extensions
)

__source_config() {
  if [[ -d "${1}" ]]; then
    local config_file
    for config_file ("${1}"/*.zsh(N)); do
      source "${config_file}"
    done
  elif [[ -f "${1}" ]]; then
    source "${1}"
  else
    printf 'Could not find configs for %s\n' "${1}"
  fi
}

for config ("${zsh_configs[@]}"); do
  __source_config "${config}"
done
unset config
