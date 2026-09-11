# GPG and Keybase helpers
#
# Sets GPG_TTY for pinentry, provides key export/import functions,
# and clipboard integration for SSH public keys.

# only set GPG_TTY when we actually have a terminal
if [[ -t 1 ]]; then
  GPG_TTY=$(tty)
  export GPG_TTY
fi

if has gpg-connect-agent; then
  gpg-clear-cache() {
    gpg-connect-agent reloadagent /bye
  }
fi

if has gpg; then
  gpg-pubkey() {
    local id=${1:-matt@smallwat3r.com}
    gpg --armor --export "$id"
  }

  gpg-list-keys() {
    gpg --list-keys --keyid-format=short
  }

  # send secret subkeys and trust to another host over ssh, the primary
  # key never leaves this machine. the host must be a full tailnet name
  # (see TS_DOMAINS) so the key only ever travels over Tailscale.
  # usage: gpg-send-subkeys <host>.<tailnet domain> [id]
  gpg-send-subkeys() {
    [[ "$1" == ?*.(${(~j:|:)TS_DOMAINS}) ]] || {
      echo "usage: gpg-send-subkeys <host>.(${(j:|:)TS_DOMAINS}) [id]" >&2
      return 1
    }
    local id=${2:-matt@smallwat3r.com}
    gpg --armor --export-secret-subkeys "$id" | ssh "$1" 'gpg --import' &&
      gpg --export-ownertrust | ssh "$1" 'gpg --import-ownertrust'
  }

  # copy the GPG SSH key to the clipboard.
  gpg-ssh-key() {
    local key
    key="$(gpg --export-ssh-key "$USER")" || return
    printf "%s" "$key" | clip && echo 'Key copied to clipboard!'
  }
fi

if has keybase; then
  gpg-keybase-import() { keybase pgp export | gpg --import - }
  gpg-keybase-import-secret() {
    keybase pgp export --secret | gpg --allow-secret-key-import --import -
  }
fi
