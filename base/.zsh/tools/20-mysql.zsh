if [[ -d /usr/local/opt/mysql-client/bin ]]; then
  if (( ${path[(Ie)/usr/local/opt/mysql-client/bin]} == 0 )); then
    path+=('/usr/local/opt/mysql-client/bin')
  fi
fi
