# SSH helpers

has ssh || return

# When a host legitimately regenerates its key, ssh refuses with
# "REMOTE HOST IDENTIFICATION HAS CHANGED". This catches that, and
# on confirmation drops the stale known_hosts entry and reconnects,
# where ssh prompts for the new fingerprint as usual. Any other
# failure is passed through untouched.
ssh() {
  # nomultios, or zsh would copy stdout into the pipe as well as fd 3.
  # localtraps so the cleanup trap does not leak into the shell.
  setopt localoptions nomultios localtraps
  local err host rc
  # Remote hosts have no terminfo for Emacs's Eat terminal
  [[ $TERM == eat-* ]] && local -x TERM=xterm-256color
  err=$(mktemp)
  trap 'rm -f "$err"' EXIT
  { command ssh "$@" 2>&1 >&3 | tee "$err" >&2; } 3>&1
  rc=${pipestatus[1]}
  host=$(awk '/^Host key for .* has changed/ {print $4}' "$err")
  if [[ -n $host ]] && read -q "?Forget stored key for $host and reconnect? [y/N] "; then
    echo
    ssh-keygen -R "$host" && command ssh "$@"
    rc=$?
  fi
  return $rc
}
