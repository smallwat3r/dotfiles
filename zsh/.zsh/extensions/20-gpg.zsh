export GPG_TTY=$(tty)

if [ -f /usr/local/bin/keybase ]; then
  gpg-keybase-import() {
    keybase pgp export | gpg --import
  }

  gpg-keybase-import-secret() {
    keybase pgp export --secret | gpg --import --allow-secret-key-import
  }
fi
