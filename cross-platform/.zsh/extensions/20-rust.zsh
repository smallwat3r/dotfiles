if [ -d "${HOME}"/.cargo/bin ]; then
  export PATH="${PATH:+${PATH}:}${HOME}/.cargo/bin"
fi
