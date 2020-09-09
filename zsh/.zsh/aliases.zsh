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

# Misc and general
alias \
  .....="cd ../../../.." \
  ....="cd ../../.." \
  ...="cd ../.." \
  ..="cd .." \
  batt="bluebatt" \
  c="clear" \
  cd..="cd .." \
  ctags="/usr/local/bin/ctags" \
  diskspace="df -P -kHl" \
  dots="cd $HOME/dotfiles" \
  fonts="open $HOME/Library/Fonts" \
  l="ls -pFf" \
  ll="ls -la" \
  ls.="ls -pF -d .*" \
  ls="ls -pF" \
  lss="ls -la | grep --color=never '\->'" \
  mkdir="mkdir -pv" \
  myip="curl http://ipecho.net/plain; echo" \
  p="pwd" \
  pihole="ssh pi 'pihole status'" \
  pitemp="ssh pi 'vcgencmd measure_temp'" \
  pubip="curl http://ipecho.net/plain; echo" \
  qq="exit" \
  sk="sketch" \
  sl="ls -pF" \
  ww="vifm ."

# Editing stuff
alias \
  e="$EDITOR" \
  eala="$EDITOR $HOME/.config/alacritty/alacritty.yml" \
  eali="$EDITOR $HOME/.aliases" \
  edit="$EDITOR" \
  efun="$EDITOR $HOME/.functions" \
  egit="$EDITOR $HOME/.config/git/config" \
  ekit="$EDITOR $HOME/.config/kitty/kitty.conf" \
  etmu="$EDITOR $HOME/.config/tmux/tmux.conf" \
  evim="$EDITOR $HOME/.config/nvim/init.vim" \
  ezsh="$EDITOR $HOME/.zshrc" \
  svi="sudo nvim" \
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
