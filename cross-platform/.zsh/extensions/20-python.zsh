# Python related configuration

# ensure to create all Poetry virtual env within the project
export POETRY_VIRTUALENVS_IN_PROJECT=true

if [ -d "${HOME}/.poetry/bin" ]; then
  export PATH="${HOME}/.poetry/bin:${PATH}"
fi

# YOLO
export PIP_BREAK_SYSTEM_PACKAGES=1

# activate the nearest python venv
avenv() {
  local dir
  dir="$(pwd)"
  while [[ ! -f "${dir}/.venv/bin/activate" && -n "${dir}" ]]; do
    dir="${dir%/*}"
  done
  if [[ -f "${dir}/.venv/bin/activate" ]]; then
    echo "Activating virtualenv from ${dir}/.venv"
    # shellcheck disable=SC1091
    source "${dir}/.venv/bin/activate"
  else
    echo 'No .venv found'
    false
  fi
}

# run python from the nearest venv
vpython() {
  avenv && python "$@"
}
