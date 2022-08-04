if [ -d '/usr/local/opt/grep/libexec/gnubin' ]; then
  export PATH="${PATH:+${PATH}:}/usr/local/opt/grep/libexec/gnubin"

  # Ensure gnu-grep is being used instead of system's /usr/bin/grep
  alias \
    grep='/usr/local/opt/grep/libexec/gnubin/grep' \
    egrep='/usr/local/opt/grep/libexec/gnubin/egrep' \
    fgrep='/usr/local/opt/grep/libexec/gnubin/fgrep'
fi
