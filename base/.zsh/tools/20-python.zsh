# Python development
#
# Poetry PATH setup and virtualenv helpers. avenv activates the nearest
# .venv up the directory tree. vrun/vpython/vpip run commands in venv.

# Add Poetry's bin dir to PATH (old and new install locations)
path_prepend "$HOME/.poetry/bin"
path_prepend "$HOME/.local/bin"

# Activate the nearest python venv (.venv) up the directory tree
avenv() {
  local dir=$PWD
  while :; do
    if [[ -f "$dir/.venv/bin/activate" ]]; then
      echo "Activating virtualenv from $dir/.venv"
      # shellcheck source=/dev/null
      source "$dir/.venv/bin/activate"
      return 0
    fi
    [[ "$dir" == / ]] && break
    dir=$(dirname "$dir")
  done
  echo "No .venv found" >&2
  return 1
}

# Activate venv if needed
_avenv_ensure() { [[ -z "$VIRTUAL_ENV" ]] && avenv || return 0 }
# Run command in venv
vrun() { _avenv_ensure && "$@" }

alias vpython='vrun python'
alias vpip='vrun pip'
alias vpytest='vrun pytest'
