if [ -f /usr/local/share/antigen/antigen.zsh ]; then
  source '/usr/local/share/antigen/antigen.zsh'

  antigen bundle zsh-users/zsh-autosuggestions
  antigen bundle hlissner/zsh-autopair
  antigen bundle zdharma-continuum/fast-syntax-highlighting

  antigen apply
fi

# TODO: fix this as this doesn't seem to work out of the box
if [ ! -d "${FAST_WORK_DIR}" ]; then
  fast-theme --quiet sv-orple
fi
