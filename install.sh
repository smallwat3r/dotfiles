#!/usr/bin/env bash
# Matthieu Petiteau <mpetiteau.pro@gmail.com>
#
# Manage dotfiles (symlinks and cask/brew installs).
#
# SYMLINKS
# --------
# Dotfiles symlinks are stored under the file ``symlink`` (<source>  :  <destination>)
# Add new symlinks using the same method
#
# CASKS / BREW
# ------------
# Casks are stored under ``./files/cask`` and brew are store under ``./files/brew``
# Append the lists to add new casks or brew utils
set -e

_install_brew() {
  brew ls --versions $1 >/dev/null &&
    printf '%s%s\n' \
      "∙ $1 seems already installed " \
      "(version $(brew ls --versions $1))." &&
    return

  printf "› Installing $1 ...\n"
  brew install $1 >/dev/null 2>&1
}

_install_cask() {
  brew cask list $1 >/dev/null &&
    printf "∙ $1 seems already installed via Homebrew.\n" &&
    return

  local _name=$(echo $1 | sed -E 's/[-_]+/ /g')
  ls /Applications | grep -ri "$_name" >/dev/null &&
    printf "∙ $1 seems already installed in /Applications.\n" &&
    return

  printf "› Installing $1 ...\n"
  brew cask install $1 >/dev/null 2>&1
}

_symlink() {
  local _source=$1
  local _destination=${2/\~\//$HOME\/}

  printf "› Symlink $_source to $_destination ...\n"

  mkdir -p $(dirname "$_destination")
  ln -sf "$(pwd)/$_source" $_destination

  if [[ $_source =~ ^bin.* ]]; then
    chmod 0755 $_destination
  fi
}

# If needed upgrade bash version to be able to use readarray
command -v readarray >/dev/null 2>&1 || _install_brew bash

readarray -t _BREW <./files/brew
for _br in "${_BREW[@]}"; do
  _install_brew $_br
done

readarray -t _CASK <./files/cask
for _ca in "${_CASK[@]}"; do
  _install_cask $_ca
done

readarray -t _SYMLINK <symlink
for _sym in "${_SYMLINK[@]}"; do
  _sour=$(echo "${_sym%%:*}" | xargs)
  _dest=$(echo "${_sym##*:}" | xargs)
  _symlink $_sour $_dest
done
