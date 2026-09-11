#!/usr/bin/env bash
#
# bootstrap.sh - provision a fresh Fedora install from these dotfiles
#
# Enables the third-party repositories, installs everything listed in
# packages.dnf.txt and packages.flatpak.txt, installs the Ocrab Nerd Font
# (github.com/smallwat3r/ocrab-font), enables the system services those
# packages need, stows the dotfiles, then sets up the Emacs config
# (github.com/smallwat3r/emacs), firefox-sway
# (github.com/smallwat3r/firefox-sway) and the QMK CLI. Safe to re-run,
# every step is idempotent.
#
# Usage: ./bootstrap.sh
#
set -euo pipefail

cd "$(dirname "$(realpath "$0")")"

FEDORA=$(rpm -E %fedora)

COPRS=(
  erikreider/swayosd     # swayosd
  erovia/dfu-programmer  # dfu-programmer
  tofik/nwg-shell        # nwg-look, nwg-displays, cliphist
)

REPO_FILES=(
  https://download.docker.com/linux/fedora/docker-ce.repo
  https://pkgs.tailscale.com/stable/fedora/tailscale.repo
  https://rpm.releases.hashicorp.com/fedora/hashicorp.repo  # terraform
  https://repo.ivpn.net/stable/fedora/generic/ivpn.repo
  https://negativo17.org/repos/fedora-multimedia.repo      # steam, displaylink
)

SERVICES=(
  clamav-freshclam
  docker
  earlyoom
  input-remapper
  ivpn-service
  pcscd.socket  # smart card daemon, needed by yubikey-manager
  tailscaled
)

GROUPS_=(docker vboxusers)

step() { printf '\n\033[1;34m==> %s\033[0m\n' "$1"; }
warn() { printf '\033[1;33m!!  %s\033[0m\n' "$1" >&2; }

installed() { rpm -q "$1" >/dev/null 2>&1; }

add_repo() {
  sudo dnf config-manager addrepo --overwrite "$@"
}

enable_repos() {
  step 'Enabling repositories'

  installed rpmfusion-free-release || sudo dnf install -y \
    "https://mirrors.rpmfusion.org/free/fedora/rpmfusion-free-release-$FEDORA.noarch.rpm" \
    "https://mirrors.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$FEDORA.noarch.rpm"

  installed terra-release || sudo dnf install -y \
    --repofrompath "terra,https://repos.fyralabs.com/terra$FEDORA" \
    --setopt "terra.gpgkey=https://repos.fyralabs.com/terra$FEDORA/key.asc" \
    terra-release

  # Installing the keybase rpm also installs its repository
  installed keybase || sudo dnf install -y https://prerelease.keybase.io/keybase_amd64.rpm

  local repo
  for repo in "${REPO_FILES[@]}"; do
    add_repo --from-repofile="$repo"
  done

  add_repo --id=google-cloud-cli \
    --set=name='Google Cloud CLI' \
    --set=baseurl=https://packages.cloud.google.com/yum/repos/cloud-sdk-el10-x86_64 \
    --set=gpgkey=https://packages.cloud.google.com/yum/doc/rpm-package-key-v10.gpg \
    --set=gpgcheck=1

  add_repo --id=balena-etcher \
    --set=name='balena-etcher' \
    --set=baseurl="https://dl.cloudsmith.io/public/balena/etcher/rpm/fedora/$FEDORA/\$basearch" \
    --set=gpgkey=https://dl.cloudsmith.io/public/balena/etcher/gpg.70528471AFF9A051.key \
    --set=gpgcheck=1 \
    --set=repo_gpgcheck=1

  local copr
  for copr in "${COPRS[@]}"; do
    sudo dnf -y copr enable "$copr"
  done
}

install_packages() {
  step 'Installing dnf packages'
  awk '{print $1}' packages.dnf.txt | xargs sudo dnf install -y

  step 'Installing flatpaks'
  sudo flatpak remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo
  sudo flatpak remote-modify --no-filter flathub
  awk '{print $1}' packages.flatpak.txt | xargs flatpak install -y flathub
}

install_fonts() {
  step 'Installing fonts'
  local dir="$HOME/.local/share/fonts"
  mkdir -p "$dir"
  curl -fsSL -o "$dir/ocrab-nerd-font.otf" \
    https://github.com/smallwat3r/ocrab-font/releases/latest/download/ocrab-nerd-font.otf
  fc-cache -f "$dir"
}

enable_services() {
  step 'Enabling system services'
  sudo systemctl enable --now "${SERVICES[@]}"

  step 'Adding user to groups'
  sudo usermod -aG "$(IFS=,; echo "${GROUPS_[*]}")" "$USER"
}

stow_dotfiles() {
  step 'Stowing dotfiles'
  make stow
}

clone() { [ -d "$2" ] || git clone "https://github.com/smallwat3r/$1.git" "$2"; }

install_emacs() {
  step 'Installing Emacs config'
  clone emacs ~/.config/smallwat3r-emacs
  # make stow enables emacs.service, so an Emacs may already have started
  # and created an empty ~/.emacs.d by the time we get here. Testing -e
  # alone reads that as "already set up" and skips the symlink, which
  # leaves Emacs running against an empty config, so clear it out first.
  if [ -d ~/.emacs.d ] && [ ! -L ~/.emacs.d ] && ! rmdir ~/.emacs.d 2>/dev/null; then
    warn "$HOME/.emacs.d exists and is not empty, leaving it alone"
    return 0
  fi
  if [ ! -e ~/.emacs.d ] && [ ! -L ~/.emacs.d ]; then
    ln -s ~/.config/smallwat3r-emacs ~/.emacs.d
  fi
  # Packages are installed by elpaca on first start, emacs.service is
  # enabled by make stow and does that on next login
}

install_firefox_wm() {
  step 'Installing firefox-sway'
  clone firefox-sway ~/code/firefox-sway
  # firefox-sway links into the default profile, which only exists once
  # Firefox has been started at least once
  [ -f ~/.mozilla/firefox/profiles.ini ] || \
    firefox --headless --screenshot /dev/null about:blank >/dev/null 2>&1 || true
  make -C ~/code/firefox-sway install
}

install_qmk() {
  step 'Installing QMK'
  # The toolchains (arm-none-eabi-*, avr-*, dfu-*) come from
  # packages.dnf.txt, the CLI itself is only on PyPI. It installs under
  # ~/.local/lib/python3.X, so re-run this after a Fedora upgrade bumps
  # the system Python or `qmk` dies with "No module named 'qmk_cli'".
  python3 -m pip install --user --upgrade qmk
  # Clones ~/qmk_firmware, syncs its submodules and runs qmk doctor.
  # Skip it once the clone exists, re-running would reset it. It only
  # warns about the udev rules, so install them ourselves, they let
  # `qmk flash` reach the boards without sudo.
  [ -d ~/qmk_firmware ] || qmk setup -y
  sudo install -m 644 ~/qmk_firmware/util/udev/50-qmk.rules /etc/udev/rules.d/
  sudo udevadm control --reload-rules
}

enable_repos
install_packages
install_fonts
enable_services
stow_dotfiles
install_emacs
install_firefox_wm
install_qmk

step 'Done'
cat <<'EOF'
Remaining manual steps:
  - log out and back in for the new group memberships to apply
  - yubikey-pam-setup      register the YubiKey for sudo and login
  - tailscale up           join the tailnet
  - git clone https://nas.ts.smallwat3r.com/git/smallwat3r/notes.git ~/notes
                           after the tailnet and a token from `make forgejo-token`
                           in the homelab repo, notes-sync.timer fails until then
  - rbw login              Bitwarden CLI, used by the launcher
  - keybase login
  - WEB_EXT_API_KEY=... WEB_EXT_API_SECRET=... make -C ~/code/firefox-sway install
                           install the Tabs to Windows extension from AMO
EOF
