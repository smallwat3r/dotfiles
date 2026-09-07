# shellcheck shell=bash
# Shared helpers for the img-* scripts
# Source this file: . "${HOME}/.local/lib/img.sh"

format_size() {
  numfmt --to=iec "$1" 2>/dev/null || echo "${1}B"
}

filesize() {
  stat -c%s "$1" 2>/dev/null || stat -f%z "$1"
}
