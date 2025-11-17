# Prefer GNU grep over BSD grep when available

if [[ -d /usr/local/opt/grep/libexec/gnubin ]]; then
  # Intel macOS Homebrew path
  if (( ${path[(Ie)/usr/local/opt/grep/libexec/gnubin]} == 0 )); then
    path=("/usr/local/opt/grep/libexec/gnubin" $path)
  fi
elif [[ -d /opt/homebrew/opt/grep/libexec/gnubin ]]; then
  # Apple Silicon macOS Homebrew path
  if (( ${path[(Ie)/opt/homebrew/opt/grep/libexec/gnubin]} == 0 )); then
    path=("/opt/homebrew/opt/grep/libexec/gnubin" $path)
  fi
fi
