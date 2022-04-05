if [ -f /usr/local/bin/docker ]; then

  __docker_ensure_pattern() {
    if [[ -z "$1" ]]; then
      printf 'Please specify an pattern.\n'
      exit 1
    fi
  }

  dps() {
    docker ps --format '{{.ID}} ¬¬¬ {{.Image}} ¬¬¬ {{.Names}} ¬¬¬ {{.Status}}' \
      | column -t -s '¬¬¬' -c "$(tput cols)"
  }

  dpsa() {
    docker ps -a --format '{{.ID}} ¬¬¬ {{.Image}} ¬¬¬ {{.Names}} ¬¬¬ {{.Status}}' \
      | column -t -s '¬¬¬' -c "$(tput cols)"
  }

  dpsq() {
    docker ps -q
  }

  dprune() {
    printf 'y' | docker system prune
  }

  dlog() {
    (__docker_ensure_pattern "$1" \
      && docker logs --follow "$(docker ps | grep "$1" 2>/dev/null | cut -d ' ' -f1)" 2>/dev/null \
      || printf 'Could not match [%s]\n' "$1" >&2)
  }

  dexe() {
    (__docker_ensure_pattern "$1" \
      && docker exec -it "$(docker ps | grep "$1" 2>/dev/null | cut -d ' ' -f1)" "${2:-/bin/sh}" 2>/dev/null \
      || printf 'Could not match [%s]\n' "$1" >&2)
  }

  dip() {
    (__docker_ensure_pattern "$1" \
      && docker inspect --format '{{range.NetworkSettings.Networks}}{{.IPAddress}}{{end}}' \
        "$(docker ps | grep "$1" 2>/dev/null | cut -d ' ' -f1)" 2>/dev/null \
      || printf 'Could not match [%s]\n' "$1" >&2)
  }

  dstop() {
    docker ps --filter name="${1}" --filter status=running -aq | xargs docker stop
  }

  docker-rmq() {
    docker run -d --hostname my-rabbit -p 5672:5672 -p 8080:15672 rabbitmq:3-management
  }

  docker-mongo() {
    docker run -d -p 27000:27017 -v "$HOME/.dockervolumes/mongo/db:/data/db" mongo
  }

  docker-redis() {
    docker run -d -p 6379:6379 redis
  }
fi
