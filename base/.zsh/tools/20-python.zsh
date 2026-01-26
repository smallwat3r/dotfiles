# Python related configuration

# Add Poetry's bin dir to PATH (old and new install locations)
for _poetry_bin in "$HOME/.poetry/bin" "$HOME/.local/bin"; do
  if [[ -d "$_poetry_bin" ]]; then
    if (( ${path[(Ie)$_poetry_bin]} == 0 )); then
      path=("$_poetry_bin" $path)
    fi
  fi
done
unset _poetry_bin

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

# Only walk the tree if we're not already inside a venv
_avenv_ensure() {
  if [[ -z "$VIRTUAL_ENV" ]]; then
    avenv || return 1
  fi
}

# Run python from the (nearest) activated venv
vpython() {
  _avenv_ensure && python "$@"
}

# Run pip from the (nearest) activated venv
vpip() {
  _avenv_ensure && pip "$@"
}

# Run pytest from the (nearest) activated venv
vpytest() {
  _avenv_ensure && pytest "$@"
}
