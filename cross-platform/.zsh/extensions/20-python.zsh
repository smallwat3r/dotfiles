# Python related configuration

# Ensure Poetry virtualenvs are created in the project directory
export POETRY_VIRTUALENVS_IN_PROJECT=true

# Add Poetry's bin dir to PATH
if [[ -d "$HOME/.poetry/bin" ]]; then
  if (( ${path[(Ie)$HOME/.poetry/bin]} == 0 )); then
    path=("$HOME/.poetry/bin" $path)
  fi
fi

# YOLO
export PIP_BREAK_SYSTEM_PACKAGES=1

# Activate the nearest python venv (.venv) up the directory tree
avenv() {
  local dir=$PWD
  while [[ ! -f "$dir/.venv/bin/activate" && "$dir" != "/" ]]; do
    dir=${dir%/*}
  done
  if [[ -f "$dir/.venv/bin/activate" ]]; then
    echo "Activating virtualenv from $dir/.venv"
    source "$dir/.venv/bin/activate"
  else
    echo 'No .venv found'
    return 1
  fi
}

# Run python from the nearest venv
vpython() {
  avenv && python "$@"
}
