if [[ -d $HOME/balena/bin ]]; then
  if (( ${path[(Ie)$HOME/balena/bin]} == 0 )); then
    path+=("$HOME/balena/bin")
  fi
fi

alias bal='balena'
alias bald='balena device list'
alias balf='balena fleet list'
alias balp='balena push'
alias balv='balena version'

_bal_select_device() {
  local jq_fmt='\(.uuid)\t\(.device_name)\t\(.status)\t\(.device_type)'
  jq_fmt+='\t\(.belongs_to__application[0].app_name)'
  balena device list --json \
    | jq -r ".[] | \"$jq_fmt\"" \
    | fzf --with-nth=2.. \
    | cut -f1
}

bal-ssh() {
  local device=$(_bal_select_device)
  [[ -n "$device" ]] && balena ssh "$device" "$@"
}

bal-logs() {
  local device=$(_bal_select_device)
  [[ -n "$device" ]] && balena device logs "$device" --tail "$@"
}

bal-tunnel() {
  local device=$(_bal_select_device)
  [[ -n "$device" ]] && balena tunnel "$device" "$@"
}
