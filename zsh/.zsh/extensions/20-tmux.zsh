if [ -f /usr/local/bin/tmux ]; then
  tksa() {
    tmux kill-session -a
  }

  tks() {
    tmux kill-session -t
  }

  tls() {
    tmux list-sessions
  }

  sp() {
    tmux splitw -v
  }

  vs() {
    tmux splitw -h
  }
fi
