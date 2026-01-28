# Docker helpers
#
# Formatted container/image listings, quick exec/logs access, and
# cleanup commands. dps/dim for pretty output, dlog/dexe for quick access.

has docker || return

_docker_cols() {
  tput cols 2>/dev/null || echo 120
}

_docker_ps() {
  local fmt='{{.ID}} ¬¬¬ {{.Image}} ¬¬¬ {{.Names}} ¬¬¬ {{.Status}} ¬¬¬ {{.Ports}}'
  docker ps "$@" --format "$fmt" | column -t -s '¬¬¬' -c "$(_docker_cols)"
}

alias dps='_docker_ps'
alias dpsa='_docker_ps -a'

dpsq() {
  docker ps -q
}

dim() {
  docker images --format '{{.Repository}} ¬¬¬ {{.Tag}} ¬¬¬ {{.ID}} ¬¬¬ {{.Size}}' \
    | column -t -s '¬¬¬' -c "$(_docker_cols)"
}

dprune() {
  local all=0
  [[ $1 == "-a" ]] && all=1
  docker system prune --force
  (( all )) && docker volume prune --force && docker image prune -a --force
}

dstop() {
  local ids
  ids=$(docker ps -q)
  [[ -n $ids ]] && docker stop $ids || echo "No running containers"
}

drm() {
  local ids
  ids=$(docker ps -aq)
  [[ -n $ids ]] && docker rm $ids || echo "No containers to remove"
}

dlog() {
  if [[ -z $1 ]]; then
    echo "Usage: dlog <container> [lines]" >&2
    return 1
  fi
  docker inspect "$1" &>/dev/null || { echo "Container '$1' not found" >&2; return 1; }
  docker logs --follow --tail "${2:-10}" "$1"
}

dexe() {
  if [[ -z $1 ]]; then
    echo "Usage: dexe <container> [shell]" >&2
    return 1
  fi
  docker inspect "$1" &>/dev/null || { echo "Container '$1' not found" >&2; return 1; }
  docker exec -it "$1" "${2:-/bin/sh}"
}

dip() {
  if [[ -z $1 ]]; then
    echo "Usage: dip <container>" >&2
    return 1
  fi
  docker inspect -f '{{range.NetworkSettings.Networks}}{{.IPAddress}}{{end}}' "$1"
}

dstat() {
  docker stats --no-stream --format \
    '{{.Name}} ¬¬¬ {{.CPUPerc}} ¬¬¬ {{.MemUsage}} ¬¬¬ {{.NetIO}}' \
    | column -t -s '¬¬¬' -c "$(_docker_cols)"
}
