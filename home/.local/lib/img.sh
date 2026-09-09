# shellcheck shell=bash
# Shared helpers for the img-* scripts
# Source this file: . "${HOME}/.local/lib/img.sh"

format_size() { numfmt --to=iec "$1"; }

filesize() { stat -c%s "$1"; }
