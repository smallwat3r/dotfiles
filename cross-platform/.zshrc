# smallwat3r's ZSH config entrypoint

if (( ${+DEBUG_ZSH_PERF} )); then
  zmodload zsh/zprof
fi

ZSH_ROOT="${HOME}/.zsh"

if [ -d "${ZSH_ROOT}" ]; then
  # load configs from core and extensions directories
  for dir in "core" "extensions"; do
    local config_path="${ZSH_ROOT}/${dir}"
    if [ -d "${config_path}" ]; then
      for file in "${config_path}"/*.zsh(N); do
        source "${file}"
      done
    else
      printf 'Could not find configs in %s\n' "${config_path}"
    fi
  done
  unset dir config_path file

  # load functions
  fpath=("${ZSH_ROOT}"/functions $fpath)
  autoload -U "${ZSH_ROOT}"/functions/*(:t)
else
  printf "ZSH_ROOT not found at %s\n" "${ZSH_ROOT}"
fi


# load private configs
if [[ -f "${HOME}/.zshrc.private" ]]; then
  source "${HOME}/.zshrc.private"
fi

if (( ${+DEBUG_ZSH_PERF} )); then
  zprof
fi
