if [ -f /usr/local/share/antigen/antigen.zsh ]; then
  source '/usr/local/share/antigen/antigen.zsh'
fi

if ! command -v antigen >/dev/null; then
  antigen bundle zsh-users/zsh-autosuggestions
  antigen bundle zsh-users/zsh-syntax-highlighting

  antigen apply
fi
