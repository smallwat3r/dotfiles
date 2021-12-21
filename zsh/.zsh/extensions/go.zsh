export GOPATH="$HOME/go"

if [[ ! "$PATH" == "*/$GOPATH/bin*" ]]; then
  export PATH="${PATH:+${PATH}:}$GOPATH/bin"
fi

