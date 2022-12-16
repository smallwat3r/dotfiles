if [ -f /usr/local/bin/docker ]; then

  # Shorter alternative to `docker ps`
  dps() {
    docker ps --format '{{.ID}} ¬¬¬ {{.Image}} ¬¬¬ {{.Names}} ¬¬¬ {{.Status}}' \
      | column -t -s '¬¬¬' -c "$(tput cols)"
  }

  # Shorter alternative to `docker ps -a`
  dpsa() {
    docker ps -a --format '{{.ID}} ¬¬¬ {{.Image}} ¬¬¬ {{.Names}} ¬¬¬ {{.Status}}' \
      | column -t -s '¬¬¬' -c "$(tput cols)"
  }

  # List all running container image ids
  # This is handy to use in command substitution like: docker stop $(dpsq)
  dpsq() {
    docker ps -q
  }

  # Docker prune
  dprune() {
    printf 'y' | docker system prune
  }

  # Show container logs
  dlog() {
    docker logs --follow --tail 10 "${1}"
  }

  # Exec into container (use /bin/sh as default if no shell if specified)
  dexe() {
    docker exec -it "${1}" "${2:-/bin/sh}"
  }

  # Print out container IP address
  dip() {
    docker inspect --format '{{range.NetworkSettings.Networks}}{{.IPAddress}}{{end}}' "${1}"
  }
fi
