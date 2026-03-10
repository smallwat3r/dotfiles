# Go toolchain
#
# Adds GOPATH/bin to PATH for installed Go binaries.

export GOTOOLCHAIN=auto

path_add "$GOPATH/bin"
