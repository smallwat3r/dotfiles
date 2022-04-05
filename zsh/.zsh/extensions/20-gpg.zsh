export GPG_TTY=$(tty)

if [ -f /usr/local/bin/gpg-connect-agent ]; then
  clear-cache-gpg-password() {
    gpg-connect-agent reloadagent /bye
  }
fi

if [ -f /usr/local/bin/gpg ]; then
  gpg-pub-key() {
    gpg --armor --export mpetiteau.pro@gmail.com
  }

  gpg-list-keys() {
    gpg --list-secret-keys --keyid-format LONG
  }
fi

if [ -f /usr/local/bin/keybase ]; then
  gpg-keybase-import() {
    keybase pgp export | gpg --import
  }

  gpg-keybase-import-secret() {
    keybase pgp export --secret | gpg --import --allow-secret-key-import
  }
fi
