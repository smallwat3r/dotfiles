export GPG_TTY=$(tty)

if [ -f /usr/local/bin/gpg-connect-agent ]; then
  # Clear the gpg authentication cache. Next time using GPG, it will ask for
  # the password.
  clear-cache-gpg-password() {
    gpg-connect-agent reloadagent /bye
  }
fi

if [ -f /usr/local/bin/gpg ]; then
  # Display public key.
  gpg-pub-key() {
    gpg --armor --export mpetiteau.pro@gmail.com
  }

  # List all GPG keys.
  gpg-list-keys() {
    gpg --list-secret-keys --keyid-format LONG
  }
fi

if [ -f /usr/local/bin/keybase ]; then
  # Import GPG key from keybase.
  gpg-keybase-import() {
    keybase pgp export | gpg --import
  }

  # Import GPG secret from keybase.
  gpg-keybase-import-secret() {
    keybase pgp export --secret | gpg --import --allow-secret-key-import
  }
fi
