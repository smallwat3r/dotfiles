# Minimal syntax highlighting using region_highlight
#
# Highlights unknown commands (red), quoted strings (yellow),
# sudo (purple), and rm (dim bold on entire line).
# No external dependencies.

__syntax_hl() {
  emulate -L zsh
  region_highlight=()

  local buf=$BUFFER len=${#BUFFER}
  (( len == 0 )) && return

  # -- quoted strings (yellow) --
  local i=1 q
  while (( i <= len )); do
    q=${buf[i]}
    if [[ $q == "'" || $q == '"' ]]; then
      local start=$i
      (( i++ ))
      while (( i <= len )) && [[ ${buf[i]} != "$q" ]]; do
        (( i++ ))
      done
      region_highlight+=(
        "$(( start - 1 )) $i fg=yellow"
      )
    fi
    (( i++ ))
  done

  # -- first word (skip leading whitespace) --
  local cmd
  cmd=${buf##[[:space:]]#}
  cmd=${cmd%%[[:space:]]*}
  [[ -z $cmd ]] && return

  local offset=$(( len - ${#${buf##[[:space:]]#}} ))
  local cmd_end=$(( offset + ${#cmd} ))

  # rm: dim bold on the whole line
  if [[ $cmd == rm ]]; then
    region_highlight+=("0 $len fg=90,bold")
    return
  fi

  # sudo: purple bold
  if [[ $cmd == sudo ]]; then
    region_highlight+=(
      "$offset $cmd_end fg=164,bold"
    )
    return
  fi

  # unknown command: red bold
  if ! whence -- "$cmd" >/dev/null 2>&1; then
    region_highlight+=(
      "$offset $cmd_end fg=red,bold"
    )
  fi
}

zle -N zle-line-pre-redraw __syntax_hl
