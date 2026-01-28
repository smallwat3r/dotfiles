path_add "$HOME/balena/bin"

has balena || return

alias bal='balena'
alias bald='balena device list'
alias balf='balena fleet list'
alias balp='balena push'
alias balv='balena version'

_bal_select_device() {
  has jq fzf || { echo "jq and fzf required" >&2; return 1; }
  local jq_fmt='\(.uuid)\t\(.device_name)\t\(.status)\t\(.device_type)'
  jq_fmt+='\t\(.belongs_to__application[0].app_name)'
  local devices
  devices=$(balena device list --json 2>/dev/null) || { echo "Failed to list devices" >&2; return 1; }
  jq -r ".[] | \"$jq_fmt\"" <<< "$devices" | fzf --with-nth=2.. | cut -f1
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
