if [[ -d $HOME/balena/bin ]]; then
  if (( ${path[(Ie)$HOME/balena/bin]} == 0 )); then
    path+=("$HOME/balena/bin")
  fi
fi
