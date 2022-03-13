# Move up directories easily
# i.e.:
#   ...  becomes ../..
#   .... becomes ../../..
#   etc...

__rationalise-dot() {
  [[ $LBUFFER = *.. ]] && LBUFFER+=/.. || LBUFFER+=.
}

zle -N __rationalise-dot
bindkey "." __rationalise-dot
