if [ -f /usr/local/bin/kubectl ]; then
  # Monitor kube's pods memory
  monitor-pods() {
    if [[ -z "$1" ]]; then
      printf 'Please specify a namespace.\n'
    else
      while true; do
        kubectl -n "$1" top pods --use-protocol-buffers
        sleep 1
      done
    fi
  }
fi
