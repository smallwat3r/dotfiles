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
  nvim=$HOME/.config/nvim

# Global aliases
alias -g \
  G="| grep" \
  L="| less"

# General
alias \
  c="clear" \
  p="pwd" \
  l="ls -pFf" \
  ll="ls -la" \
  ls.="ls -pF -d .*" \
  ls="ls -pF" \
  sl="ls -pF" \
  lss="ls -la | grep --color=never '\->'" \
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
  pif="pip3.8 freeze" \
  pii="pip3.8 install" \
  pip="pip3.8" \
  py="python3.8" \
  python2="python2.7" \
  python="python3.8" \
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

# Misc stuff
alias \
  batt="bluebatt" \
  ctags="/usr/local/bin/ctags" \
  diskspace="df -P -kHl" \
  dots="cd $HOME/dotfiles" \
  fonts="open $HOME/Library/Fonts" \
  mkdir="mkdir -pv" \
  myip="curl http://ipecho.net/plain; echo" \
  pihole="ssh pi 'pihole status'" \
  pitemp="ssh pi 'vcgencmd measure_temp'" \
  pubip="curl http://ipecho.net/plain; echo" \
  sk="sketch" \
  ww="vifm ." \
  mongo-docker="docker run -d -p 27000:27017 -v $HOME/.dockervolumes/mongo/db:/data/db mongo" \
  kube="kubectl"
