# Docker helper functions
# Dependencies: docker

if (( $+commands[docker] )); then
  _docker_cols() {
    local cols
    cols=$(tput cols 2>/dev/null || echo 120)
    echo "$cols"
  }

  # Shorter alternative to `docker ps`
  dps() {
    docker ps --format '{{.ID}} ¬¬¬ {{.Image}} ¬¬¬ {{.Names}} ¬¬¬ {{.Status}}' \
      | column -t -s '¬¬¬' -c "$(_docker_cols)"
  }

  # Shorter alternative to `docker ps -a`
  dpsa() {
    docker ps -a --format '{{.ID}} ¬¬¬ {{.Image}} ¬¬¬ {{.Names}} ¬¬¬ {{.Status}}' \
      | column -t -s '¬¬¬' -c "$(_docker_cols)"
  }

  # List all running container IDs (usable as: docker stop $(dpsq))
  dpsq() {
    docker ps -q
  }

  # Docker prune (remove unused data)
  dprune() {
    docker system prune --force
  }

  # Show container logs
  dlog() {
    if [[ -z $1 ]]; then
      echo "Usage: dlog <container>" >&2
      return 1
    fi
    docker logs --follow --tail 10 "$1"
  }

  # Exec into container (use /bin/sh by default if no shell specified)
  dexe() {
    if [[ -z $1 ]]; then
      echo "Usage: dexe <container> [shell]" >&2
      return 1
    fi
    docker exec -it "$1" "${2:-/bin/sh}"
  }

  # Print container IP address
  dip() {
    if [[ -z $1 ]]; then
      echo "Usage: dip <container>" >&2
      return 1
    fi
    docker inspect --format '{{range.NetworkSettings.Networks}}{{.IPAddress}}{{end}}' "$1"
  }
fi
