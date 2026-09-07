# IVPN helpers
#
# Aliases for common commands, fuzzy server selection, and a fix for
# IVPN's kill switch blocking Tailscale (see ivpn-allow-tailscale).

has ivpn || return

alias ivs='ivpn status'
alias ivc='ivpn connect -f'
alias ivl='ivpn connect -last'
alias ivd='ivpn disconnect'
alias ivfw='ivpn firewall'
alias ivpause='ivpn connection -pause'
alias ivresume='ivpn connection -resume'
alias ivx='ivpn exclude'

# Fuzzy pick a WireGuard server and connect to it
iv-connect() {
  has fzf || { echo "fzf required" >&2; return 1; }
  local host
  host=$(ivpn servers -p wg | tail -n +2 \
    | fzf --delimiter='|' --with-nth=2,3,4,5 | awk -F'|' '{gsub(/ /, "", $2); print $2}')
  [[ -n $host ]] && ivpn connect -p wg "$host" "$@"
}

# Let Tailscale traffic through IVPN's kill switch. IVPN's firewall
# only allows the VPN tunnel, loopback and (optionally) RFC1918 LAN,
# so the 100.64.0.0/10 tailnet range and MagicDNS get dropped.
# Persistent setting, only needs to be run once. Allow LAN is a
# separate call as it is only wanted on trusted networks.
iv-allow-tailscale() {
  ivpn firewall -exceptions '100.64.0.0/10, fd7a:115c:a1e0::/48'
}

# IVPN's policy routing rules sit ahead of Tailscale's and would send
# tailnet traffic into the IVPN tunnel. The tailscaled systemd drop-in
# adds these routes at start, but 'tailscale down' flushes them.
iv-tailscale-routes() {
  sudo ip route replace 100.64.0.0/10 dev tailscale0 \
    && sudo ip -6 route replace fd7a:115c:a1e0::/48 dev tailscale0
}
