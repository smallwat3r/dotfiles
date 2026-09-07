#!/usr/bin/env bash
#
# bootstrap.sh - provision a fresh Fedora install from these dotfiles
#
# Enables the third-party repositories, installs everything listed in
# packages.dnf.txt and packages.flatpak.txt, enables the system services
# those packages need, then stows the dotfiles. Safe to re-run, every
# step is idempotent.
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

enable_repos
install_packages
enable_services
stow_dotfiles

step 'Done'
cat <<'EOF'
Remaining manual steps:
  - log out and back in for the new group memberships to apply
  - yubikey-pam-setup      register the YubiKey for sudo and login
  - tailscale up           join the tailnet
  - rbw login              Bitwarden CLI, used by the launcher
  - keybase login
EOF
