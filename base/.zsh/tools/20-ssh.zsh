# SSH helpers

has ssh || return

# When a host legitimately regenerates its key, ssh refuses with
# "REMOTE HOST IDENTIFICATION HAS CHANGED". This catches that, and
# on confirmation drops the stale known_hosts entry and reconnects,
# where ssh prompts for the new fingerprint as usual. Any other
# failure is passed through untouched.
ssh() {
  # nomultios, or zsh would copy stdout into the pipe as well as fd 3
  setopt localoptions nomultios
  local err host rc
  err=$(mktemp)
  { command ssh "$@" 2>&1 >&3 | tee "$err" >&2; } 3>&1
  rc=${pipestatus[1]}
  host=$(awk '/^Host key for .* has changed/ {print $4}' "$err")
  rm -f "$err"
  if [[ -n $host ]] && read -q "?Forget stored key for $host and reconnect? [y/N] "; then
    echo
    ssh-keygen -R "$host" && command ssh "$@"
    rc=$?
  fi
  return $rc
}
