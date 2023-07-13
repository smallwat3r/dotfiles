# ZSH plugins
# Plugins are managed by Antigen
# curl -L git.io/antigen >/usr/local/share/antigen/antigen.zsh

if [ -f /usr/local/share/antigen/antigen.zsh ]; then
  source '/usr/local/share/antigen/antigen.zsh'

  antigen bundle zsh-users/zsh-autosuggestions
  antigen bundle hlissner/zsh-autopair
  antigen bundle zsh-users/zsh-syntax-highlighting

  antigen apply
fi
