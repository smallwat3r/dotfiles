# Tailscale VPN helpers
#
# Aliases for common commands and fuzzy device selection for SSH,
# ping, and file transfer. ts-switch for multi-account support.

is_macos && alias tailscale='/Applications/Tailscale.app/Contents/MacOS/Tailscale'

has tailscale || return

alias ts='tailscale'
alias tss='tailscale status'
alias tsip='tailscale ip'
alias tsup='tailscale up'
alias tsdn='tailscale down'
alias tsnc='tailscale netcheck'

_ts_select_device() {
  has jq fzf || { echo "jq and fzf required" >&2; return 1; }
  local status
  status=$(tailscale status --json 2>/dev/null) || { echo "Failed to get status" >&2; return 1; }
  jq -r '.Peer[] | "\(.DNSName | split(".")[0])\t\(.TailscaleIPs[0])\t\(.Online)"' <<< "$status" \
    | fzf --with-nth=1.. \
    | cut -f1
}

ts-ssh() {
  local user=$1 device=$(_ts_select_device)
  [[ -n $device ]] && ssh "${user:+$user@}$device"
}

ts-ping() {
  local device=$(_ts_select_device)
  [[ -n $device ]] && tailscale ping "$device" "$@"
}

ts-send() {
  local device=$(_ts_select_device)
  [[ -n $device ]] && tailscale file cp "$@" "${device}:"
}

ts-switch() {
  local -A accounts=(
    work  d52c
    home  a195
  )
  if [[ -z $1 ]]; then
    echo "Usage: ts-switch <alias>"
    echo "Available aliases: ${(k)accounts}"
    return 1
  fi
  local account_id=${accounts[$1]}
  if [[ -z $account_id ]]; then
    echo "Unknown alias: $1"
    echo "Available aliases: ${(k)accounts}"
    return 1
  fi
  sudo tailscale switch "$account_id"
}
