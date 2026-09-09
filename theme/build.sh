#!/bin/sh
# Generate per-app colour configs from theme/palette.
set -eu
cd "$(dirname "$0")/.."
. ./theme/palette

vars() { grep -E '^[a-z_][a-z0-9_]*=' theme/palette; }

# Sway: variables picked up by config.d/*.conf (included in order)
{
  echo '# Generated from theme/palette by theme/build.sh - do not edit.'
  vars | while IFS== read -r k v; do echo "set \$$k #$v"; done
} > home/.config/sway/config.d/00-theme.conf

# Foot: included from foot.ini
{
  echo '# Generated from theme/palette by theme/build.sh - do not edit.'
  echo '[colors]'
  echo "foreground=$term_fg"
  echo "background=$term_bg"
  echo "selection-foreground=$black"
  echo "selection-background=$face"
  vars | while IFS== read -r k v; do
    case $k in
      ansi[0-7]) echo "regular${k#ansi}=$v" ;;
      ansi*) echo "bright$(( ${k#ansi} - 8 ))=$v" ;;
    esac
  done
} > home/.config/foot/theme.ini

# Tmux: sourced from tmux.conf
{
  echo '# Generated from theme/palette by theme/build.sh - do not edit.'
  echo "set -g pane-border-style 'fg=#$shadow'"
  echo "set -g pane-active-border-style 'fg=#$face'"
  echo "set -g status-style 'bg=#$focus,fg=#$term_fg'"
} > home/.config/tmux/theme.conf

# Zsh: THEME_* env vars for fzf and anything else shell-side
{
  echo '# Generated from theme/palette by theme/build.sh - do not edit.'
  vars | while IFS== read -r k v; do
    echo "export THEME_$(echo "$k" | tr '[:lower:]' '[:upper:]')='#$v'"
  done
} > home/.zsh/tools/10-palette.zsh

# Waybar: GTK CSS colour definitions, imported from style.css
{
  echo '/* Generated from theme/palette by theme/build.sh - do not edit. */'
  vars | while IFS== read -r k v; do echo "@define-color $k #$v;"; done
} > home/.config/waybar/colors.css

# Mako, swaylock and the foot launcher profile: render the whole config
# from a template, substituting {{name}} with the bare hex value.
subst() { sed "$(vars | sed 's|^\(.*\)=\(.*\)$|s/{{\1}}/\2/g|')" "$1"; }

render() {
  { echo "# Generated from theme/$(basename "$1") by theme/build.sh - edit that file instead."
    subst "$1"
  } > "$2"
  if grep -q '{{' "$2"; then
    echo "self-check failed: unresolved placeholder in $2" >&2
    exit 1
  fi
}
render theme/mako.conf.in home/.config/mako/config
render theme/swaylock.conf.in home/.config/swaylock/config
render theme/foot-launcher.ini.in home/.config/foot/launcher.ini

# Slack: theme string to paste into Preferences -> Themes (no config file exists)
# Order: column bg, menu hover, active item, active item text,
#        hover item, text, active presence, mention badge
echo "#$face,#$shadow,#$navy,#$white,#$shadow,#$black,#$ansi2,#$urgent" \
  > theme/slack.txt
