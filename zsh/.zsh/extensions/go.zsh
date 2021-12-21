if [[ ! "$PATH" == "*/$HOME/go/bin*" ]]; then
  export PATH="${PATH:+${PATH}:}$HOME/go/bin"
fi

export GOPATH="$HOME/go"
