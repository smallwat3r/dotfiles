# Rust toolchain
#
# Adds Cargo's bin directory to PATH for rustc, cargo, and installed tools.

[[ -d "$HOME/.cargo/bin" ]] && path=("$HOME/.cargo/bin" $path)
