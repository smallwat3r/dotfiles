if [[ "$OSTYPE" == 'linux-gnu' ]]; then
  if [ -f /usr/bin/pacman ]; then
    # Search pacman package and install.
    pacman-install () {
      pacman -Slq | fzf --preview 'pacman -Si {1}' | xargs -ro sudo pacman -S
    }

    # Search pacman package and uninstall.
    pacman-uninstall () {
      pacman -Qq | fzf --preview 'pacman -Qi {1}' | xargs -ro sudo pacman -Rns
    }
  fi
fi
