# Rationalise dots for easy directory navigation
#
# Typing multiple dots expands to parent directories:
#   ...  becomes ../..
#   .... becomes ../../..
# Must be loaded last for compatibility with other key bindings.

__rationalise-dot() {
  [[ $LBUFFER = *.. ]] && LBUFFER+=/.. || LBUFFER+=.
}
zle -N __rationalise-dot
bindkey "." __rationalise-dot
