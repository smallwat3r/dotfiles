# Move up directories i.e. ... >> ../.. and .... >> ../../... etc.
__rationalise-dot() {
  [[ $LBUFFER = *.. ]] && LBUFFER+=/.. || LBUFFER+=.
}
zle -N __rationalise-dot
bindkey "." __rationalise-dot
