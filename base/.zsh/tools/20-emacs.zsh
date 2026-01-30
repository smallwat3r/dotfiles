# Emacs configuration
#
# Sets up Emacs paths, Doom Emacs bin directory, and Eat terminal
# integration for shell features like directory tracking.

# Prefer a Homebrew Emacs on macOS, otherwise fall back to /usr/bin/emacs
if is_macos; then
  if [[ -x /opt/homebrew/bin/emacs ]]; then
    export EMACS='/opt/homebrew/bin/emacs'
  else
    export EMACS='emacs'
  fi
else
  export EMACS='/usr/bin/emacs'
fi
