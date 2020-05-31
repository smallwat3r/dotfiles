#!/usr/bin/env bash
# Matthieu Petiteau <mpetiteau.pro@gmail.com>
#
# Manage dotfiles (symlinks and cask/brew installs).
#
# SYMLINKS
# --------
# Dotfiles symlinks are stored under _SYMLINK (<source>:<destination>).
# Add new symlinks using the same method.
#
# CASKS / BREW
# ------------
# Casks are stored under _CASK and brew are store under _BREW.
# Append the lists to add new casks or brew utils.

set -e

_SYMLINK=(
  '.zshrc                       : ~/.zshrc'
  '.functions                   : ~/.functions'
  '.aliases                     : ~/.aliases'
  '.tmux.conf                   : ~/.tmux.conf'
  '.gitconfig                   : ~/.gitconfig'
  'misc/python/.pylintrc        : ~/.pylintrc'
  'misc/python/.isort.cfg       : ~/.isort.cfg'
  'misc/python/yapf.toml        : ~/.config/yapf/style'
  '.ripgreprc                   : ~/.ripgreprc'
  'pip/pip.conf                 : ~/.pip/pip.conf'
  '.sketchrc                    : ~/.config/.sketchrc'
  'alacritty/alacritty.yml      : ~/.config/alacritty/alacritty.yml'
  'nvim/init.vim                : ~/.config/nvim/init.vim'
  'nvim/syntax/dockerfile.vim   : ~/.config/nvim/syntax/dockerfile.vim'
  'nvim/spell/en.utf-8.add      : ~/.config/nvim/spell/en.utf-8.add'
  'nvim/spell/en.utf-8.add.spl  : ~/.config/nvim/spell/en.utf-8.add.spl'
  'nvim/dict/dockerfile.txt     : ~/.config/nvim/dict/dockerfile.txt'
  'nvim/dict/go.txt             : ~/.config/nvim/dict/go.txt'
  'nvim/dict/html.txt           : ~/.config/nvim/dict/html.txt'
  'nvim/dict/javascript.txt     : ~/.config/nvim/dict/javascript.txt'
  'nvim/dict/python.txt         : ~/.config/nvim/dict/python.txt'
  'nvim/dict/sql.txt            : ~/.config/nvim/dict/sql.txt'
  'bin/localip                  : /usr/local/bin/localip'
  'bin/battery                  : /usr/local/bin/battery'
  'bin/tubestatus/tubestatus    : /usr/local/bin/tubestatus'
  'bin/synonym/synonym          : /usr/local/bin/synonym'
  'bin/sketch/sketch            : /usr/local/bin/sketch'
  'bin/extract                  : /usr/local/bin/extract'
  'bin/is_venv                  : /usr/local/bin/is_venv'
  'bin/shpwd                    : /usr/local/bin/shpwd'
  'bin/git_branch               : /usr/local/bin/git_branch'
  'bin/git_root                 : /usr/local/bin/git_root'
)

_BREW=(
  tmux
  redir
  git
  python@3.8
  go
  wget
  zsh
  antigen
  fzf
  htop
  curl
  bash
  gnu-sed
  nmap
  jq
  make
  fd
  the_silver_searcher
  ripgrep
  neovim
  node
  asciinema
  imagemagick
  gifsicle
  tidy-html5
  shfmt
  coreutils
  diff-so-fancy
)

_CASK=(
  sequel-pro
  docker
  alacritty
  slack
  ngrok
  alfred
  thunderbird
  google-chrome
)

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

  local _name=$(
    echo $1 |
      sed -E 's/[-_]+/ /g'
  )
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

for _br in "${_BREW[@]}"; do
  _install_brew $_br
done

for _ca in "${_CASK[@]}"; do
  _install_cask $_ca
done

for _sym in "${_SYMLINK[@]}"; do
  _sour=$(echo "${_sym%%:*}" | xargs)
  _dest=$(echo "${_sym##*:}" | xargs)
  _symlink $_sour $_dest
done