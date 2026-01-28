# Homebrew settings
#
# Disables analytics and auto-update for faster brew commands.

is_macos && {
  export HOMEBREW_NO_ANALYTICS=1
  export HOMEBREW_NO_AUTO_UPDATE=1
}
