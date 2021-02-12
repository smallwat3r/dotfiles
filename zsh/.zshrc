# ZSH config
# ~~~~~~~~~~

_configs=(
  'settings.zsh'
  'aliases.zsh'
  'functions.zsh'
  'plugins.zsh'
  'prompt.zsh'
  'completion.zsh'
  'vterm.zsh'
)

for file in $_configs; do
  [[ -f "$HOME/.zsh/$file" ]] && source "$HOME/.zsh/$file" end
done
