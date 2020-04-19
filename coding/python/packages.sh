#!/usr/bin/env bash
# Install python packages

PYTHON_PACKAGES=(
  black
  pylint
  sqlparse
  jsbeautifier
  pynvim
  bandit
)
pip3 install ${PYTHON_PACKAGES[@]}
