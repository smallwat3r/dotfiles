#!/usr/bin/env bash
# Install python packages

printf 'Installing python packages...\n'

PYTHON_PACKAGES=(
  black
  pylint
  sqlparse
  jsbeautifier
  pynvim
  bandit
)
pip3 install ${PYTHON_PACKAGES[@]} >/dev/null 2>&1
