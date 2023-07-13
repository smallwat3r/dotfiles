# Tmux helper functions
# Dependencies: tmux

if [ -f /usr/local/bin/tmux ] || [ -f /usr/bin/tmux ]; then
  # Kill all Tmux sessions.
  tksa() {
    tmux kill-session -a
  }

  # Kill current Tmux session.
  tks() {
    tmux kill-session -t
  }

  # List all Tmux sessions.
  tls() {
    tmux list-sessions
  }

  # Split current Tmux window horizontally.
  sp() {
    tmux splitw -v
  }

  # Split current Tmux window vertically.
  vs() {
    tmux splitw -h
  }
fi
