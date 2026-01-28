# GNU grep on macOS
#
# Prepends Homebrew's GNU grep to PATH so it takes precedence over
# BSD grep. Supports both Intel and Apple Silicon Homebrew paths.

path_prepend /usr/local/opt/grep/libexec/gnubin   # Intel macOS
path_prepend /opt/homebrew/opt/grep/libexec/gnubin # Apple Silicon
