if [ -f /usr/local/share/antigen/antigen.zsh ]; then
  source '/usr/local/share/antigen/antigen.zsh'

  antigen bundle zsh-users/zsh-autosuggestions
  antigen bundle zsh-users/zsh-syntax-highlighting

  antigen apply
fi
