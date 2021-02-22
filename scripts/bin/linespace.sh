#!/usr/bin/env bash

# Modified version from the original script from
# https://gist.github.com/chrissimpkins/f39e85f5f157d839e46168de1b61a174

# //////////////////////////////////////////////////////////////////////
#
# linespace.sh
#  A shell script that modifies all .otf and .ttf fonts in the
#  working directory to $PERCENT_UPM% UPM line spacing
#  Copyright 2018 Christopher Simpkins
#  MIT License
#
#  Dependency: font-line (https://github.com/source-foundry/font-line)
#              install project from PyPI with: `pip install font-line`
#
#  Usage: Modify PERCENT_UPM to the desired value, save, then:
#         $ chmod +x linespace.sh
#         $ ./linespace.sh
#
# //////////////////////////////////////////////////////////////////////

# Define the percent UPM value for the line spacing modification below
PERCENT_UPM="$1"

[[ -z "$PERCENT_UPM" ]] && {
  printf "Please specify a line spacing value\n"
  exit 1
}

# confirm font-line install, if not present install it
if which font-line; then
  echo "font-line executable detected"
else
  pip install font-line
fi

# modify all .ttf and .otf files in working directory with defined % UPM line spacing
font-line percent $PERCENT_UPM *.ttf *.otf
