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
  'files/zshrc                       : ~/.zshrc'
  'files/functions                   : ~/.functions'
  'files/aliases                     : ~/.aliases'
  'files/tmux.conf                   : ~/.tmux.conf'
  'files/gitconfig                   : ~/.gitconfig'
  'files/code/python/pylintrc        : ~/.pylintrc'
  'files/code/python/mypy.ini        : ~/.mypy.ini'
  'files/code/python/isort.cfg       : ~/.isort.cfg'
  'files/code/python/yapf.toml       : ~/.config/yapf/style'
  'files/code/pip/pip.conf           : ~/.pip/pip.conf'
  'files/ripgreprc                   : ~/.ripgreprc'
  'files/sketchrc                    : ~/.config/.sketchrc'
  'files/alacritty/alacritty.yml     : ~/.config/alacritty/alacritty.yml'
  'files/nvim/init.vim               : ~/.config/nvim/init.vim'
  'files/nvim/syntax/dockerfile.vim  : ~/.config/nvim/syntax/dockerfile.vim'
  'files/nvim/spell/en.utf-8.add     : ~/.config/nvim/spell/en.utf-8.add'
  'files/nvim/spell/en.utf-8.add.spl : ~/.config/nvim/spell/en.utf-8.add.spl'
  'files/nvim/dict/dockerfile.txt    : ~/.config/nvim/dict/dockerfile.txt'
  'files/nvim/dict/go.txt            : ~/.config/nvim/dict/go.txt'
  'files/nvim/dict/html.txt          : ~/.config/nvim/dict/html.txt'
  'files/nvim/dict/javascript.txt    : ~/.config/nvim/dict/javascript.txt'
  'files/nvim/dict/python.txt        : ~/.config/nvim/dict/python.txt'
  'files/nvim/dict/sql.txt           : ~/.config/nvim/dict/sql.txt'
  'files/bin/localip                 : /usr/local/bin/localip'
  'files/bin/battery                 : /usr/local/bin/battery'
  'files/bin/tubestatus/tubestatus   : /usr/local/bin/tubestatus'
  'files/bin/synonym/synonym         : /usr/local/bin/synonym'
  'files/bin/sketch/sketch           : /usr/local/bin/sketch'
  'files/bin/extract                 : /usr/local/bin/extract'
  'files/bin/is_venv                 : /usr/local/bin/is_venv'
  'files/bin/shpwd                   : /usr/local/bin/shpwd'
  'files/bin/git_branch              : /usr/local/bin/git_branch'
  'files/bin/git_root                : /usr/local/bin/git_root'
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
