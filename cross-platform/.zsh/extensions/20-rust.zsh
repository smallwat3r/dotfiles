if (( ${path[(Ie)$HOME/.cargo/bin]} == 0 )); then
  path+=("$HOME/.cargo/bin")
fi

[[ -f "$HOME/.cargo/env" ]] && source "$HOME/.cargo/env"
