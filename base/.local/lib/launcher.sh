# Shared library for launcher scripts
# Source this file: . "${HOME}/.local/lib/launcher.sh"

# Helpers

die() { echo "$1" >&2; exit 1; }

has() { command -v "$1" >/dev/null 2>&1; }

require() { for cmd in "$@"; do has "$cmd" || die "$cmd is required"; done; }

# OS detection

is_macos() { [[ "$(uname -s)" == "Darwin" ]]; }

is_linux() { [[ "$(uname -s)" == "Linux" ]]; }

is_wayland() { [[ -n "${WAYLAND_DISPLAY:-}" ]]; }

is_wlroots() { [[ -n "${SWAYSOCK:-}" ]] || [[ -n "${HYPRLAND_INSTANCE_SIGNATURE:-}" ]]; }

# FZF configuration

if is_macos; then
    FZF_BIND="alt-left:backward-word,alt-right:forward-word,alt-bs:backward-kill-word,home:first,end:last"
else
    FZF_BIND="ctrl-left:backward-word,ctrl-right:forward-word,ctrl-bs:backward-kill-word,home:first,end:last"
fi

FZF_COLORS="bg:#222222,fg:#bbbbbb,bg+:#005577,fg+:#eeeeee,hl:#eeeeee,hl+:#eeeeee,pointer:#eeeeee,prompt:#eeeeee,info:#eeeeee,gutter:#222222"

# FZF picker with standard options
# Usage: fzf_pick [prompt]
fzf_pick() {
    fzf --reverse --wrap --tiebreak=index \
        --pointer='' --prompt="${1:-}" \
        --highlight-line --no-separator --no-scrollbar --info=inline-right \
        --bind="$FZF_BIND" --color="$FZF_COLORS"
}

# Clipboard operations

clip() {
    if has wl-copy; then
        wl-copy
    elif has xclip; then
        xclip -selection clipboard
    elif has pbcopy; then
        pbcopy
    else
        die "No clipboard tool found (wl-copy, xclip, pbcopy)"
    fi
}

clip_clear() {
    local delay="${1:-45}"
    if has wl-copy; then
        (sleep "$delay" && wl-copy --clear) &
    elif has xclip; then
        (sleep "$delay" && xclip -selection clipboard < /dev/null) &
    elif has pbcopy; then
        (sleep "$delay" && pbcopy < /dev/null) &
    fi
}

# Copy content and auto-clear after delay
# Usage: echo "secret" | clip_secure [delay]
clip_secure() {
    local delay="${1:-45}"
    clip
    clip_clear "$delay"
}
