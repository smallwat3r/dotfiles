# Docker helper functions
# Dependencies: docker

(( $+commands[docker] )) || return

_docker_cols() {
  local cols
  cols=$(tput cols 2>/dev/null || echo 120)
  echo "$cols"
}

dps() {
  docker ps --format '{{.ID}} ¬¬¬ {{.Image}} ¬¬¬ {{.Names}} ¬¬¬ {{.Status}}' \
    | column -t -s '¬¬¬' -c "$(_docker_cols)"
}

dpsa() {
  docker ps -a --format '{{.ID}} ¬¬¬ {{.Image}} ¬¬¬ {{.Names}} ¬¬¬ {{.Status}}' \
    | column -t -s '¬¬¬' -c "$(_docker_cols)"
}

dpsq() {
  docker ps -q
}

dprune() {
  docker system prune --force
}

dlog() {
  if [[ -z $1 ]]; then
    echo "Usage: dlog <container>" >&2
    return 1
  fi
  docker logs --follow --tail 10 "$1"
}

dexe() {
  if [[ -z $1 ]]; then
    echo "Usage: dexe <container> [shell]" >&2
    return 1
  fi
  docker exec -it "$1" "${2:-/bin/sh}"
}

dip() {
  if [[ -z $1 ]]; then
    echo "Usage: dip <container>" >&2
    return 1
  fi
  docker inspect --format '{{range.NetworkSettings.Networks}}{{.IPAddress}}{{end}}' "$1"
}
