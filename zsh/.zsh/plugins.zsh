# Zsh plugins
# ~~~~~~~~~~~

# homebrew
export HOMEBREW_NO_ANALYTICS=1
export HOMEBREW_NO_AUTO_UPDATE=1

# ripgrep
export RIPGREP_CONFIG_PATH="$HOME/.config/.ripgreprc"

# fzf
[[ -f "$HOME/.fzf.zsh" ]] && source "$HOME/.fzf.zsh"

export FZF_DEFAULT_OPTS='
  --height 96% --reverse --border
  --color dark,hl:37,hl+:37,bg+:#101010,fg+:136
  --color info:136,prompt:37,spinner:136,pointer:230,marker:230
'
export FZF_DEFAULT_COMMAND='rg --files --hidden --follow -g "!__pycache__/" -g "!.git/"'

# antigen
[[ -f '/usr/local/share/antigen/antigen.zsh' ]] && source '/usr/local/share/antigen/antigen.zsh'

antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-autosuggestions
antigen bundle hlissner/zsh-autopair

antigen apply

# zsh auto-suggestions colors
export ZSH_AUTOSUGGEST_USE_ASYNC=true
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=37'
export ZSH_AUTOSUGGEST_STRATEGY=(history completion)
