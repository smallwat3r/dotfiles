# Tmux session management
#
# Quick shortcuts for session control: tks (kill), tksa (kill all),
# tls (list), sp/vs (split panes).

has tmux || return

tksa() { tmux kill-session -a }  # Kill all sessions except current
tks() { tmux kill-session }      # Kill current session
tls() { tmux list-sessions }     # List all sessions
sp() { tmux split-window -v }    # Split pane horizontally
vs() { tmux split-window -h }    # Split pane vertically
