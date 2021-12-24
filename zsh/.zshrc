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
unsetopt BEEP               # do no beep on errors
unsetopt LIST_BEEP          # do not beep on anbiguous completion

zsh_root="${HOME}/.zsh"

# Load functions
fpath=("${zsh_root}"/functions $fpath)
autoload -U "${zsh_root}"/functions/*(:t)

# Load config files
zsh_configs=(
  "${zsh_root}"/prompt.zsh
  "${zsh_root}"/aliases
  "${zsh_root}"/utils
  "${zsh_root}"/extensions
)

__source_config() {
  if [[ -d "${1}" ]]; then
    for file in "${1}"/*.zsh; do
      source "${file}"
    done
  elif [[ -f "${1}" ]]; then
    source "${1}"
  else
    printf 'Could not find configs for %s\n' "${1}"
  fi
}

for config in "${zsh_configs[@]}"; do
  __source_config "${config}"
done
