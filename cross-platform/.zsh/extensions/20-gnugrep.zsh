# Ensure gnu-grep is being used instead of system's /usr/bin/grep

if [ -d '/usr/local/opt/grep/libexec/gnubin' ]; then
  export PATH="${PATH:+${PATH}:}/usr/local/opt/grep/libexec/gnubin"

  alias \
    grep='/usr/local/opt/grep/libexec/gnubin/grep' \
    egrep='/usr/local/opt/grep/libexec/gnubin/egrep' \
    fgrep='/usr/local/opt/grep/libexec/gnubin/fgrep'
elif [ -d '/opt/homebrew/bin' ]; then
  # it's possible homebrew installs those elsewhere
  alias \
    grep='/opt/homebrew/bin/ggrep' \
    egrep='/opt/homebrew/bin/gegrep' \
    fgrep='/opt/homebrew/bin/fegrep'
fi

export GREP_OPTIONS='--color=auto'
export GREP_COLOR='0;31'
