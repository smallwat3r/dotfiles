export POETRY_VIRTUALENVS_IN_PROJECT=true

if [ -d "${HOME}/.poetry/bin" ]; then
  export PATH="${HOME}/.poetry/bin:${PATH}"
fi
