#!/usr/bin/env bash
# Install python packages

printf 'Installing python packages...\n'

PYTHON_PACKAGES=(
  yapf
  black
  pylint
  sqlparse
  isort
  jsbeautifier
  pynvim
  bandit
)
pip3 install ${PYTHON_PACKAGES[@]} >/dev/null 2>&1
