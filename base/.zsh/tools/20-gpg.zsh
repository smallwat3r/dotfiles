# GPG configs and helper functions
# Dependencies: gpg, keybase

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

  # copy the GPG SSH key to the clipboard.
  gpg-ssh-key() {
    local key
    key="$(gpg --export-ssh-key "$USER")" || return
    printf "%s" "$key" | clip && echo 'Key copied to clipboard!'
  }
fi

if has keybase; then
  gpg-keybase-import() {
    keybase pgp export | gpg --import -
  }

  gpg-keybase-import-secret() {
    keybase pgp export --secret | gpg --allow-secret-key-import --import -
  }
fi
