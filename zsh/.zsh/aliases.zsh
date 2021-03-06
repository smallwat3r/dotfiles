# Zsh aliases
# ~~~~~~~~~~~

# Directory shortcuts
hash -d \
  d=$HOME/dotfiles \
  g=$HOME/Github \
  dw=$HOME/Downloads \
  de=$HOME/Desktop \
  ssh=$HOME/.ssh \
  zsh=$HOME/.zsh \
  tmux=$HOME/.config/tmux \
  nvim=$HOME/.config/nvim \
  doom=$HOME/.doom.d \
  fonts=$HOME/Library/Fonts \
  dots=$HOME/dotfiles

# Global aliases
alias -g \
  G="| grep" \
  L="| less" \
  NE="2> /dev/null"

# General
alias \
  ...="cd ../.." \
  ....="cd ../../.." \
  .....="cd ../../../.." \
  c="clear" \
  p="pwd" \
  l="ls -pFf" \
  ll="ls -la" \
  ls.="ls -pF -d .*" \
  ls="ls -pF" \
  sl="ls -pF" \
  lss="ls -la | grep --color=never '\->'" \
  mkdir="mkdir -pv" \
  qq="exit"

# Editing stuff
alias \
  e="$EDITOR" \
  edit="$EDITOR" \
  eala="$EDITOR $HOME/.config/alacritty/alacritty.yml" \
  eali="$EDITOR $HOME/.zsh/aliases.zsh" \
  efun="$EDITOR $HOME/.zsh/functions.zsh" \
  egit="$EDITOR $HOME/.config/git/config" \
  ekit="$EDITOR $HOME/.config/kitty/kitty.conf" \
  etmu="$EDITOR $HOME/.config/tmux/tmux.conf" \
  evim="$EDITOR $HOME/.config/nvim/init.vim" \
  ezsh="$EDITOR $HOME/.zsh" \
  svi="sudo nvim" \
  ec="emacsclient" \
  v="nvim" \
  vi="nvim" \
  vidiff="nvim -d" \
  vim="nvim" \
  vimdiff="nvim -d"

# Python stuff
alias \
  pif="pip3.9 freeze" \
  pii="pip3.9 install" \
  pip="pip3.9" \
  py="python3.9" \
  python2="python2.7" \
  python="python3.9" \
  venv="python -m venv env"

# Tmux stuff
alias \
  sp="tmux splitw -v" \
  tks="tmux kill-session -t" \
  tksa="tmux kill-session -a" \
  tls="tmux list-sessions" \
  vs="tmux splitw -h"

# Git stuff
alias \
  g="git" \
  ga="git add" \
  gb="git branch" \
  gc!="git commit -v --amend" \
  gc="git commit -v" \
  gcb="git checkout -b" \
  gcq="git commit -m 'Quick commit'" \
  gd="git diff" \
  gp="git push" \
  gs="git status"

# Kube
alias \
  k="kubectl" \
  kt="kubetail" \
  kgn="kubectl get namespaces"

# gpg
alias \
  gpg-pub-key="gpg --armor --export mpetiteau.pro@gmail.com" \
  gpg-list-keys="gpg --list-secret-keys --keyid-format LONG"

# Misc stuff
alias \
  batt="bluebatt" \
  ctags="/usr/local/bin/ctags" \
  diskspace="df -P -kHl" \
  dots="cd $HOME/dotfiles" \
  fonts="open $HOME/Library/Fonts" \
  sk="sketch" \
  ww="vifm ."
