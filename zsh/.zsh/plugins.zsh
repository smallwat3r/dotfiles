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
  --color dark,hl:202,hl+:202,bg+:#101010,fg+:10
  --color info:10,prompt:202,spinner:10,pointer:10,marker:10
'
export FZF_DEFAULT_COMMAND='rg --files --hidden --follow -g "!__pycache__/" -g "!.git/"'

# antigen
[[ -f '/usr/local/share/antigen/antigen.zsh' ]] && source '/usr/local/share/antigen/antigen.zsh'

antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-autosuggestions
antigen bundle hlissner/zsh-autopair
antigen bundle skywind3000/z.lua

antigen apply

# zsh syntax-highlight options
# ----------------------------
# By default only main is activated
export ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern cursor)

# Custom colors
typeset -A ZSH_HIGHLIGHT_STYLES
export ZSH_HIGHLIGHT_STYLES[command]='fg=green,bold'
export ZSH_HIGHLIGHT_STYLES[builtin]='fg=green,bold'
export ZSH_HIGHLIGHT_STYLES[function]='fg=cyan,bold'
export ZSH_HIGHLIGHT_STYLES[global-alias]='fg=cyan,bold'
export ZSH_HIGHLIGHT_STYLES[suffix-alias]='fg=cyan,bold'
export ZSH_HIGHLIGHT_STYLES[alias]='fg=cyan,bold'
export ZSH_HIGHLIGHT_STYLES[globbing]='fg=magenta,bold'
export ZSH_HIGHLIGHT_STYLES[redirection]='fg=magenta,bold'

# zsh auto-suggestions options
# ----------------------------
export ZSH_AUTOSUGGEST_USE_ASYNC=true
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=241'
export ZSH_AUTOSUGGEST_STRATEGY=(history completion)
