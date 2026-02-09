# Minimal syntax highlighting using region_highlight
#
# Highlights unknown commands (red), quoted strings (yellow),
# sudo (purple), and rm (dim bold on entire line).
# No external dependencies.

__syntax_hl() {
  emulate -L zsh

  # Skip if buffer unchanged (redraws without edits).
  [[ $BUFFER == "${__syntax_hl_prev-}" ]] && return
  __syntax_hl_prev=$BUFFER
  region_highlight=()

  (( $#BUFFER )) || return

  # -- first word (skip leading whitespace) --
  local cmd=${BUFFER##[[:space:]]#}
  local -i offset=$(( $#BUFFER - $#cmd ))
  cmd=${cmd%%[[:space:]]*}
  [[ -n $cmd ]] || return

  local -i cmd_end=$(( offset + $#cmd ))

  # rm: dim bold on the whole line
  if [[ $cmd == rm ]]; then
    region_highlight+=("0 $#BUFFER fg=90,bold")
    return
  fi

  # sudo: purple bold
  if [[ $cmd == sudo ]]; then
    region_highlight+=("$offset $cmd_end fg=164,bold")
    return
  fi

  # unknown command: red bold
  if ! whence -- "$cmd" >/dev/null 2>&1; then
    region_highlight+=("$offset $cmd_end fg=red,bold")
  fi

  # quoted strings - jump between quotes via (ib:n:)
  local QS="'" QD='"'
  local -i pos=1 sq dq next close
  while (( pos <= $#BUFFER )); do
    sq=${BUFFER[(ib:$pos:)$QS]}
    dq=${BUFFER[(ib:$pos:)$QD]}
    if (( sq < dq )); then
      next=$sq
      close=${BUFFER[(ib:$((next + 1)):)$QS]}
    elif (( dq <= $#BUFFER )); then
      next=$dq
      close=${BUFFER[(ib:$((next + 1)):)$QD]}
    else
      break
    fi
    if (( close <= $#BUFFER )); then
      region_highlight+=(
        "$(( next - 1 )) $close fg=yellow"
      )
      pos=$(( close + 1 ))
    else
      break
    fi
  done
}

zle -N zle-line-pre-redraw __syntax_hl
