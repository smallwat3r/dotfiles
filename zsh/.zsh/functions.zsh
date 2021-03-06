# Zsh functions
# ~~~~~~~~~~~~~

# {{{1 (checkdns) check DNS records

checkdns() {
  if [[ -z "$1" ]]; then
    printf 'Please specify a host.\n'
  else
    dig @$(dig @8.8.8.8 "$1" ns +short | head -n1) "$1" ANY +noall +answer
    ping -c1 "$1"
  fi
}
# }}}1
# {{{1 (mpods) monitor kube pods memory from a namespace

mpods() {
  if [[ -z "$1" ]]; then
    printf 'Please specify a namespace.\n'
  else
    while true; do
      kubectl -n "$1" top pods
      sleep 1
    done
  fi
}

# }}}1
# {{{1 (cr) cd root of git repo

cr() {
  cd "$(git rev-parse --show-toplevel)" || exit
}

# }}}1
# {{{1 (csvpreview) preview csv files.

# source: http://stackoverflow.com/questions/1875305/command-line-csv-viewer
csvpreview() {
  sed 's/,,/, ,/g;s/,,/, ,/g' "$@" |
    column -s, -t |
    less -#2 -N -S
}

# }}}1
# {{{1 (dip) docker ip by name

dip() {
  docker inspect --format '{{ .NetworkSettings.IPAddress }}' \
    "$(docker ps |
      grep "$1" 2>/dev/null |
      cut -d ' ' -f1)" 2>/dev/null || printf 'Cannot find %s\n' "$1" >&2
}

# }}}1
# {{{1 (dlog) docker show logs by name

dlog() {
  docker logs --follow \
    "$(docker ps |
      grep "$1" 2>/dev/null |
      cut -d ' ' -f1)" 2>/dev/null || printf 'Cannot find %s\n' "$1" >&2
}

# }}}1
# {{{1 (dps) shorten docker ps

dps() {
  docker ps --format '{{.ID}} ¬¬¬ {{.Image}} ¬¬¬ {{.Names}} ¬¬¬ {{.Status}}' |
    column -t -s '¬¬¬' -c "$(tput cols)"
}

# }}}1
# {{{1 (dpsa) shorten docker ps -a

dpsa() {
  docker ps -a --format '{{.ID}} ¬¬¬ {{.Image}} ¬¬¬ {{.Names}} ¬¬¬ {{.Status}}' |
    column -t -s '¬¬¬' -c "$(tput cols)"
}

# }}}1
# {{{1 (dprune) docker prune

dprune() {
  printf 'y' | docker system prune
}

# }}}1
# {{{1 (drm) docker delete image

drm() {
  docker rmi "$1" -f
}

# }}}1
# {{{1 (dsh) docker enter in shell by name

dsh() {
  local _name
  _name=$(
    docker ps |
      grep "$1" 2>/dev/null |
      cut -d ' ' -f1
  )
  docker exec -it "$_name" /bin/sh 2>/dev/null ||
    printf 'Error finding %s\n' "$1" >&2
}

# }}}1
# {{{1 (fbr) FZF checkout git branch (including remote branches)

fbr() {
  local branches branch
  branches=$(git branch --all | grep -v HEAD)
  branch=$(
    echo "$branches" |
      fzf -d $((2 + $(wc -l <<<"$branches"))) +m
  )
  git checkout "$(
    echo "$branch" |
      sed "s/.* //" |
      sed "s#remotes/[^/]*/##"
  )"
}

# }}}1
# {{{1 (fco) FZF git commit browser

fco() {
  git log --graph --color=always --format="%C(auto)%h%d %s / %C(white)%cr by %an" "$@" |
    fzf --ansi --no-sort --reverse --tiebreak=index --bind=ctrl-s:toggle-sort --bind \
      "ctrl-m:execute: (
        grep -o '[a-f0-9]\{7\}' |
          head -1 |
          xargs -I % sh -c 'git show --color=always % |
          less -R') << 'FZF-EOF'
      {}
      FZF-EOF"
}

# }}}1
# {{{1 (fda) FZF Select a docker container to start and attach to

fda() {
  local cid
  cid=$(
    docker ps -a |
      sed 1d |
      fzf -1 -q "$1" |
      awk '{print $1}'
  )
  [ -n "$cid" ] && docker start "$cid" && docker attach "$cid"
}

# }}}1
# {{{1 (fds) FZF Select a running docker container to stop

fds() {
  local cid
  cid=$(
    docker ps |
      sed 1d |
      fzf -q "$1" |
      awk '{print $1}'
  )
  [ -n "$cid" ] && docker stop "$cid"
}

# }}}1
# {{{1 (fdrm) FZF Select a docker container to remove

fdrm() {
  local cid
  cid=$(
    docker ps -a |
      sed 1d |
      fzf -q "$1" |
      awk '{print $1}'
  )
  [ -n "$cid" ] && docker rm "$cid"
}

# }}}1
# {{{1 (fe) FZF select and open file

fe() {
  IFS=$'\n' files=($(fzf --query="$1" --preview "cat {}" --multi --select-1 --exit-0))
  [[ -n $files ]] && $EDITOR "${files[@]}"
}

# }}}1
# {{{1 (fls) List all these custom functions

fls() {
  local _pattern
  _pattern='^# {{'
  <"$HOME/.functions" |
    grep "$_pattern" |
    sed s/"$_pattern"\{1//
}

#}}}1
# {{{1 (frg) FZF fuzzy ripgrep

frg() {
  local file line
  read -r file line <<<"$(
    rg --column --line-number --no-heading --smart-case "$@" |
      fzf --query="$@" |
      awk -F: '{print $1, $2}'
  )"
  [[ -n $file ]] && $EDITOR "$file" +"$line"
}

# }}}1
# {{{1 (hs) Grep history

hs() {
  history 1 | grep "$@"
}

# }}}1
# {{{1 (hsu) Grep history (unique matches)

hsu() {
  history 1 | sort -u | grep "$@"
}

# }}}1
# {{{1 (mkd) create and enter directory

mkd() {
  mkdir -p "$@" && cd "$@" || exit
}

# }}}1
# {{{1 (o) open current dir or the one in args

o() {
  if [[ $# -eq 0 ]]; then
    open .
  else
    open "$@"
  fi
}

# }}}1
# {{{1 (pubkey) copy public key to clipboard

pubkey() {
  more "$HOME/.ssh/id_rsa.pub" | pbcopy
  printf 'Public key copied to clipboard.\n'
}

# }}}1
# {{{1 (psg) Ripgrep process

psg() {
  ps aux |
    rg -v rg |
    rg -i -e VSZ -e "$@"
}

# }}}1
# {{{1 (urls) rg - get urls

urls() {
  rg '[a-zA-Z]+://[-a-zA-Z0-9._+]+[-a-zA-Z0-9._+#=?&:;%/!~()]+'
}

# }}}1
# {{{1 (zipall) zip all individual files in current directory into a zipfile.

zipall() {
  local file
  for file in $(echo *(^/)); do
    zip "${file%.*}.zip" "$file"
  done
}

# }}}1
