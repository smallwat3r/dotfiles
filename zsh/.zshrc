# smallwat3r's ZSH config entrypoint

ZSH_ROOT="${HOME}/.zsh"

__source_config() {
  if [ -d "${1}" ]; then
    local cf
    for cf ("${1}"/*.zsh(N)); do
      source "${cf}"
    done
  else
    printf 'Could not find configs in %s\n' "${1}"
  fi
}

__main_load_zsh_config() {
  local zsh_configs=(
    core
    extensions
  )

  local config
  for config ("${zsh_configs[@]}"); do
    __source_config "${ZSH_ROOT}/${config}"
  done
}

# Load config
__main_load_zsh_config

# Load functions
fpath=("${ZSH_ROOT}"/functions $fpath)
autoload -U "${ZSH_ROOT}"/functions/*(:t)
