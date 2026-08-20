# Tmux session management
#
# Quick shortcuts for session control: tks (kill), tksa (kill all),
# tls (list), sp/vs (split panes). Also auto-starts tmux for
# interactive terminal sessions.

has tmux || return

tksa() { tmux kill-session -a }  # Kill all sessions except current
tks() { tmux kill-session }      # Kill current session
tls() { tmux list-sessions }     # List all sessions
sp() { tmux split-window -v }    # Split pane horizontally
vs() { tmux split-window -h }    # Split pane vertically

# Auto-start tmux for interactive terminal sessions, unless inside
# Emacs (which has its own window management) or Hammerspoon (macOS
# automation).
# Conditions: stdin is a tty, not already in tmux, shell is
# interactive.
if (( ! ${+INSIDE_EMACS} && ! ${+INSIDE_HS} )); then
  if [[ -t 0 && -z "$TMUX" && $- == *i* ]]; then
    exec tmux
  fi
fi
