# Docker helpers
#
# Formatted container/image listings, quick exec/logs access, and
# cleanup commands. dps/dim for pretty output, dlog/dexe for quick access.

has docker || return

_docker_ps() {
  docker ps "$@" --format 'table {{.ID}}\t{{.Image}}\t{{.Names}}\t{{.Status}}\t{{.Ports}}'
}

alias dps='_docker_ps'
alias dpsa='_docker_ps -a'

dpsq() { docker ps -q }

dim() {
  docker images --format 'table {{.Repository}}\t{{.Tag}}\t{{.ID}}\t{{.Size}}'
}

dprune() {
  local all=0
  [[ $1 == "-a" ]] && all=1
  docker system prune --force
  docker builder prune --force --keep-storage 50GB
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
  docker stats --no-stream --format 'table {{.Name}}\t{{.CPUPerc}}\t{{.MemUsage}}\t{{.NetIO}}'
}
