if [[ "$OSTYPE" =~ ^darwin ]]; then
  export HOMEBREW_NO_ANALYTICS=1
  export HOMEBREW_NO_AUTO_UPDATE=1

  if [ -d /opt/homebrew/bin ]; then
    export PATH="${PATH:+${PATH}:}/opt/homebrew/bin"
  fi
fi
