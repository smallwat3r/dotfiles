if [ -d "${HOME}"/.cargo/bin ]; then
  export PATH="${PATH:+${PATH}:}${HOME}/.cargo/bin"
fi

if [ -f "${HOME}"/.cargo/env ]; then
  source "${HOME}/.cargo/env"
fi
