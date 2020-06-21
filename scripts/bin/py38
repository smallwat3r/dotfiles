#!/usr/bin/env bash
# Set up system default python version to 3.8

_default_python() {
  ln -s -f $(which python3.8) /usr/local/bin/python
  printf 'python3.8 symlinked to default.\n' && exit 0
}

which python3.8 >/dev/null && _default_python ||
  printf 'python3.8 not found.\n' >&2 && exit 1
