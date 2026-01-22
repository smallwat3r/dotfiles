# Tailscale helper functions and aliases

if [[ "$OSTYPE" =~ ^darwin ]]; then
  alias tailscale='/Applications/Tailscale.app/Contents/MacOS/Tailscale'
fi

(( $+commands[tailscale] )) || return

alias ts='tailscale'
alias tss='tailscale status'
alias tsip='tailscale ip'
alias tsup='tailscale up'
alias tsdn='tailscale down'
alias tsnc='tailscale netcheck'

_ts_select_device() {
  tailscale status --json \
    | jq -r '.Peer[] | "\(.DNSName | split(".")[0])\t\(.TailscaleIPs[0])\t\(.Online)"' \
    | fzf --with-nth=1.. \
    | cut -f1
}

ts-ssh() {
  local user=$1
  local device=$(_ts_select_device)
  [[ -n "$device" ]] && ssh "${user:+$user@}$device"
}

ts-ping() {
  local device=$(_ts_select_device)
  [[ -n "$device" ]] && tailscale ping "$device" "$@"
}

ts-send() {
  local device=$(_ts_select_device)
  [[ -n "$device" ]] && tailscale file cp "$@" "${device}:"
}
