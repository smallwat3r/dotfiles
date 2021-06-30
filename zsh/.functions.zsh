# smallwat3r's shell functions

# Monitor kube pods memory
monitor-pods() {
  if [[ -z "$1" ]]; then
    printf 'Please specify a namespace.\n'
  else
    while true; do
      kubectl -n "$1" top pods --use-protocol-buffers
      sleep 1
    done
  fi
}

# Cd back into root of git repo
cr() {
  cd "$(git rev-parse --show-toplevel)" || exit
}

# Docker get ip
dip() {
  if [[ -z "$1" ]]; then
    printf 'Please specify an image.\n'
  else
    docker inspect --format '{{ .NetworkSettings.IPAddress }}' \
      "$(docker ps |
        grep "$1" 2>/dev/null |
        cut -d ' ' -f1)" 2>/dev/null || printf 'Cannot find %s\n' "$1" >&2
  fi
}

# Docker show logs
dlog() {
  if [[ -z "$1" ]]; then
    printf 'Please specify an image.\n'
  else
    docker logs --follow \
      "$(docker ps |
        grep "$1" 2>/dev/null |
        cut -d ' ' -f1)" 2>/dev/null || printf 'Cannot find %s\n' "$1" >&2
  fi
}

# Shorter Docker ps
dps() {
  docker ps --format '{{.ID}} ¬¬¬ {{.Image}} ¬¬¬ {{.Names}} ¬¬¬ {{.Status}}' |
    column -t -s '¬¬¬' -c "$(tput cols)"
}

# Shorter Docker ps all
dpsa() {
  docker ps -a --format '{{.ID}} ¬¬¬ {{.Image}} ¬¬¬ {{.Names}} ¬¬¬ {{.Status}}' |
    column -t -s '¬¬¬' -c "$(tput cols)"
}

# Docker system prune
dprune() {
  printf 'y' | docker system prune
}

# Docker delete image
drm() {
  if [[ -z "$1" ]]; then
    printf 'Please specify an image.\n'
  else
    docker rmi "$1" -f
  fi
}

# Docker exec into
dsh() {
  if [[ -z "$1" ]]; then
    printf 'Please specify an image.\n'
  else
    local _name
    _name=$(
      docker ps |
        grep "$1" 2>/dev/null |
        cut -d ' ' -f1
    )
    docker exec -it "$_name" /bin/sh 2>/dev/null ||
      printf 'Error finding %s\n' "$1" >&2
  fi
}

# Grep history
hs() {
  history 1 | grep "$@"
}

# Grep history unique
hsu() {
  history 1 | sort -u | grep "$@"
}

# Create and enter directory
mkd() {
  mkdir -p "$@" && cd "$@" || exit
}

# Open in finder
o() {
  if [[ $# -eq 0 ]]; then
    open .
  else
    open "$@"
  fi
}

# Copy public key to clipboard
pubkey() {
  more "$HOME/.ssh/id_rsa.pub" | pbcopy
  printf 'Public key copied to clipboard.\n'
}

# Ripgrep url pattern
urls() {
  rg '[a-zA-Z]+://[-a-zA-Z0-9._+]+[-a-zA-Z0-9._+#=?&:;%/!~()]+'
}

# Zip all files in current directory into a zipfile
zipall() {
  local file
  for file in $(echo *(^/)); do
    zip "${file%.*}.zip" "$file"
  done
}
