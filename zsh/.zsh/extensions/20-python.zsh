export POETRY_VIRTUALENVS_IN_PROJECT=true

if [ -d "${HOME}/.poetry/bin" ]; then
  export PATH="${HOME}/.poetry/bin:${PATH}"
fi

if [ -d "/Library/Frameworks/Python.framework/Versions/3.10/bin" ]; then
  export PATH="/Library/Frameworks/Python.framework/Versions/3.10/bin:${PATH}"
fi
