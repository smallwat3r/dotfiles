# shellcheck shell=bash
# Shared library for launcher scripts
# Source this file: . "${HOME}/.local/lib/launcher.sh"

# Helpers

die() { echo "$1" >&2; exit 1; }

has() { command -v "$1" >/dev/null 2>&1; }

require() { for cmd in "$@"; do has "$cmd" || die "$cmd is required"; done; }

# FZF configuration

FZF_BIND="ctrl-left:backward-word,ctrl-right:forward-word,ctrl-bs:backward-kill-word,home:first,end:last"

# Desktop palette on sway (THEME_* from theme/palette), dark elsewhere
if [[ -n "${SWAYSOCK:-}" ]]; then
    . "${HOME}/.zsh/tools/10-palette.zsh"
    FZF_COLORS="bg:$THEME_FACE,fg:$THEME_BLACK,bg+:$THEME_NAVY,fg+:$THEME_WHITE,hl:$THEME_NAVY:bold,hl+:$THEME_GOLD,pointer:$THEME_BLACK,prompt:$THEME_BLACK,info:$THEME_BLACK,gutter:$THEME_FACE,query:$THEME_BLACK"
else
    FZF_COLORS="bg:#222222,fg:#bbbbbb,bg+:#005577,fg+:#eeeeee,hl:#eeeeee,hl+:#eeeeee,pointer:#eeeeee,prompt:#eeeeee,info:#eeeeee,gutter:#222222"
fi

# FZF picker with standard options
# Usage: fzf_pick [prompt] [extra fzf options...]
fzf_pick() {
    local prompt="${1:-}"
    (( $# )) && shift
    fzf --reverse --wrap --tiebreak=index \
        --pointer='' --prompt="$prompt" \
        --highlight-line --no-separator --no-scrollbar --info=inline-right \
        --bind="$FZF_BIND" --color="$FZF_COLORS" "$@"
}

# FZF picker for tab-delimited input where first column is an ID to hide
# Usage: fzf_pick_id [prompt]
fzf_pick_id() {
    fzf_pick "${1:-}" --delimiter=$'\t' --with-nth=2..
}

# Clipboard operations

clip() { wl-copy; }

clip_clear() { (sleep "${1:-45}" && wl-copy --clear) & }

# Copy content and auto-clear after delay
# Usage: echo "secret" | clip_secure [delay]
clip_secure() {
    local delay="${1:-45}"
    clip
    clip_clear "$delay"
}
