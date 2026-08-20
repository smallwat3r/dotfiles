# Zsh login shell profile
# Sourced once at login, inherited by Sway and all GUI apps

# Chromium flags (Fedora uses this env var)
export CHROMIUM_USER_FLAGS="--enable-features=WebRTCPipeWireCapture \
    --disable-gpu-memory-buffer-video-frames"

# Go
export GOPATH="$HOME/go"

# Python/Poetry
export POETRY_VIRTUALENVS_IN_PROJECT=true
export PIP_BREAK_SYSTEM_PACKAGES=1
