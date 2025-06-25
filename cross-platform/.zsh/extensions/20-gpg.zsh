# GPG configs and helper functions
# Dependencies: gpg, keybase

export GPG_TTY=$(tty)

# Enable SSH access using GPG key
# if (( $+commands[gpgconf] )); then
#   export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
# fi

if (( $+commands[gpg-connect-agent] )); then
  # Clear the gpg authentication cache. Next time using GPG, it will ask for
  # the password.
  gpg-clear-cache() {
    gpg-connect-agent reloadagent /bye
  }
fi

if (( $+commands[gpg] )); then
  # Display public key.
  gpg-pubkey() {
    gpg --armor --export mpetiteau.pro@gmail.com
  }

  # List all GPG keys.
  gpg-list-keys() {
    gpg --list-keys --keyid-format=short
  }

  # encrypt dummy content to force the password prompt to appear
  # in order to unlock the session
  gpg-unlock () {
    echo 'foo' | gpg --clearsign >/dev/null && echo 'OK'
  }
fi

if (( $+commands[keybase] )); then
  # Import GPG key from keybase.
  gpg-keybase-import() {
    keybase pgp export | gpg --import -
  }

  # Import GPG secret from keybase.
  gpg-keybase-import-secret() {
    keybase pgp export --secret | gpg --allow-secret-key-import --import -
  }
fi
