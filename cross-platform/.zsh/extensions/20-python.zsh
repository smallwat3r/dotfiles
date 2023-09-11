# Python related configuration

# ensure to create all Poetry virtual env within the project
export POETRY_VIRTUALENVS_IN_PROJECT=true

if [ -d "${HOME}/.poetry/bin" ]; then
  export PATH="${HOME}/.poetry/bin:${PATH}"
fi

if [ -d "/Library/Frameworks/Python.framework/Versions/3.10/bin" ]; then
  export PATH="/Library/Frameworks/Python.framework/Versions/3.10/bin:${PATH}"
fi

# activate the nearest python venv
activate_nearest_venv() {
  local dir
  dir="$(pwd)"
  while [[ ! -f "${dir}/venv/bin/activate" && -n "${dir}" ]]; do
    dir="${dir%/*}"
  done
  if [[ -f "${dir}/venv/bin/activate" ]]; then
    echo "Activating virtualenv from ${dir}/venv"
    # shellcheck disable=SC1091
    source "${dir}/venv/bin/activate"
  else
    echo 'No venv found'
    false
  fi
}

# run python from the nearest venv
vpython() {
  activate_nearest_venv && python "$@"
}
