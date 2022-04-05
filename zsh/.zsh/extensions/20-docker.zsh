if [ -f /usr/local/bin/docker ]; then
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
