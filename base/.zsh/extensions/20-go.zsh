GOPATH="$HOME/go"

if (( ${path[(Ie)$GOPATH/bin]} == 0 )); then
  path+=("$GOPATH/bin")
fi

export GOPATH PATH
