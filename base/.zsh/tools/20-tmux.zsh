# Tmux session management
#
# Quick shortcuts for session control: tks (kill), tksa (kill all),
# tls (list), sp/vs (split panes).

has tmux || return

# Kill all Tmux sessions except the current one.
tksa() {
  tmux kill-session -a
}

# Kill current Tmux session.
tks() {
  tmux kill-session
}

# List all Tmux sessions.
tls() {
  tmux list-sessions
}

# Split current Tmux window horizontally.
sp() {
  tmux split-window -v
}

# Split current Tmux window vertically.
vs() {
  tmux split-window -h
}
