#!/usr/bin/env bash
#
# bootstrap.sh - provision a fresh Fedora or Debian install from these dotfiles
#
# Enables the third-party repositories, installs everything listed in
# packages.dnf.txt (Fedora) or packages.apt.txt (Debian / Raspberry Pi OS)
# and packages.flatpak.txt, installs the Ocrab Nerd Font
# (github.com/smallwat3r/ocrab-font), enables the system services those
# packages need, stows the dotfiles, then sets up the Emacs config
# (github.com/smallwat3r/emacs), claude-airlock
# (github.com/smallwat3r/claude-airlock), firefox-sway
# (github.com/smallwat3r/firefox-sway) and the QMK CLI. Safe to re-run,
# every step is idempotent.
#
# Usage: ./bootstrap.sh
#
set -euo pipefail

cd "$(dirname "$(realpath "$0")")"

# shellcheck source=/dev/null
. /etc/os-release
case "$ID" in
  fedora) DISTRO=fedora ;;
  debian|raspbian) DISTRO=debian ;;
  *) echo "Unsupported distribution: $ID" >&2; exit 1 ;;
esac

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

if [ "$DISTRO" = fedora ]; then
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
else
  SERVICES=(
    docker
    earlyoom
    input-remapper-daemon  # input-remapper.service is only an alias here
    pcscd.socket
    tailscaled
  )
  GROUPS_=(docker)
fi

step() { printf '\n\033[1;34m==> %s\033[0m\n' "$1"; }
warn() { printf '\033[1;33m!!  %s\033[0m\n' "$1" >&2; }

installed() { rpm -q "$1" >/dev/null 2>&1; }

# Package names from a manifest, skipping comments and blank lines
manifest() { awk '!/^[[:space:]]*(#|$)/ {print $1}' "$1"; }

add_repo() {
  sudo dnf config-manager addrepo --overwrite "$@"
}

enable_repos() {
  if [ "$DISTRO" = debian ]; then enable_repos_debian; else enable_repos_fedora; fi
}

enable_repos_debian() {
  step 'Enabling repositories'
  local keyring=/usr/share/keyrings/tailscale-archive-keyring.gpg
  local base="https://pkgs.tailscale.com/stable/debian/$VERSION_CODENAME"
  [ -f "$keyring" ] || curl -fsSL "$base.noarmor.gpg" | sudo tee "$keyring" >/dev/null
  curl -fsSL "$base.tailscale-keyring.list" |
    sudo tee /etc/apt/sources.list.d/tailscale.list >/dev/null
  sudo apt-get update
}

enable_repos_fedora() {
  step 'Enabling repositories'
  local FEDORA
  FEDORA=$(rpm -E %fedora)

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
  if [ "$DISTRO" = debian ]; then
    step 'Installing apt packages'
    # Take the default answer to debconf prompts (e.g. iperf3 asking to
    # run as a daemon) instead of stopping on a dialog
    manifest packages.apt.txt |
      xargs sudo DEBIAN_FRONTEND=noninteractive apt-get install -y
    install_debian_extras
    install_flatpaks
    return
  fi

  step 'Installing dnf packages'
  manifest packages.dnf.txt | xargs sudo dnf install -y

  install_flatpaks
}

install_flatpaks() {
  step 'Installing flatpaks'
  sudo flatpak remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo
  sudo flatpak remote-modify --no-filter flathub

  # Some apps are x86_64 only, skip what Flathub does not build for this
  # architecture (e.g. arm64 on a Raspberry Pi)
  local app apps=()
  while read -r app; do
    if flatpak remote-info flathub "$app" >/dev/null 2>&1; then
      apps+=("$app")
    else
      warn "$app is not available for $(flatpak --default-arch), skipping"
    fi
  done < <(manifest packages.flatpak.txt)
  [ "${#apps[@]}" -eq 0 ] || flatpak install -y flathub "${apps[@]}"
}

# Things Fedora packages that Debian does not, or names differently
install_debian_extras() {
  step 'Building sway-audio-idle-inhibit'
  if ! command -v sway-audio-idle-inhibit >/dev/null; then
    local src
    src=$(mktemp -d)
    git clone --depth 1 https://github.com/ErikReider/SwayAudioIdleInhibit.git "$src"
    meson setup "$src/build" "$src" --prefix=/usr/local --buildtype=release
    ninja -C "$src/build"
    sudo ninja -C "$src/build" install
    rm -rf "$src"
  fi

  step 'Installing rbw'
  command -v rbw >/dev/null || [ -x ~/.cargo/bin/rbw ] ||
    cargo install --locked rbw ||
    warn 'rbw failed to build (Debian rustc may be too old), try rustup'

  install_fzf
  generate_locale

  # Debian ships fd as fdfind, launcher-dir calls fd
  mkdir -p ~/.local/bin
  [ -e ~/.local/bin/fd ] || ln -s /usr/bin/fdfind ~/.local/bin/fd

  install_sway_session
}

# Debian's fzf (0.60) rejects the ctrl-left/ctrl-right/ctrl-bs key names
# the launchers and FZF_DEFAULT_OPTS bind, so take the upstream release,
# checksum-verified, into ~/.local/bin (ahead of /usr/bin on PATH).
FZF_VERSION=0.75.0

install_fzf() {
  step 'Installing fzf'
  if [ -x ~/.local/bin/fzf ] &&
     [ "$(~/.local/bin/fzf --version | cut -d' ' -f1)" = "$FZF_VERSION" ]; then
    return
  fi
  local arch tmp file base
  case "$(uname -m)" in
    aarch64) arch=arm64 ;;
    x86_64) arch=amd64 ;;
    *) warn "no fzf build for $(uname -m), keeping the packaged one"; return ;;
  esac
  tmp=$(mktemp -d)
  file="fzf-$FZF_VERSION-linux_$arch.tar.gz"
  base="https://github.com/junegunn/fzf/releases/download/v$FZF_VERSION"
  curl -fsSL -o "$tmp/$file" "$base/$file"
  curl -fsSL -o "$tmp/sums" "$base/fzf_${FZF_VERSION}_checksums.txt"
  (cd "$tmp" && grep " $file\$" sums | sha256sum -c --quiet -)
  tar -xzf "$tmp/$file" -C "$tmp" fzf
  install -D -m 755 "$tmp/fzf" ~/.local/bin/fzf
  rm -rf "$tmp"
}

# .zshenv exports LANG=en_US.UTF-8, Raspberry Pi OS only generates the
# locale picked at install (en_GB here), so glibc and GTK fall back to C.
generate_locale() {
  locale -a | grep -qix 'en_US.utf8' && return
  step 'Generating the en_US.UTF-8 locale'
  sudo sed -i 's/^# *\(en_US.UTF-8 UTF-8\)/\1/' /etc/locale.gen
  sudo locale-gen
}

# LightDM, unlike GDM, starts the session without the systemd user
# environment, so environment.d (PATH, EDITOR, SSH_AUTH_SOCK, ...) would
# never reach sway. Register a session that goes through sway-session,
# which loads it first.
install_sway_session() {
  step 'Registering the sway session'
  sudo tee /usr/share/wayland-sessions/sway-session.desktop >/dev/null <<EOF
[Desktop Entry]
Name=Sway (dotfiles)
Comment=sway with environment.d applied
Exec=$HOME/.local/bin/sway-session
Type=Application
DesktopNames=sway;wlroots
EOF
}

install_fonts() {
  step 'Installing fonts'
  local dir="$HOME/.local/share/fonts"
  mkdir -p "$dir"
  curl -fsSL -o "$dir/ocrab-nerd-font.otf" \
    https://github.com/smallwat3r/ocrab-font/releases/latest/download/ocrab-nerd-font.otf
  if [ "$DISTRO" = debian ] && ! fc-list | grep -q 'Symbols Nerd Font Mono'; then
    # Not packaged on Debian, foot and waybar fall back to it for icons
    local tmp
    tmp=$(mktemp -d)
    curl -fsSL -o "$tmp/symbols.zip" \
      https://github.com/ryanoasis/nerd-fonts/releases/latest/download/NerdFontsSymbolsOnly.zip
    unzip -o -q "$tmp/symbols.zip" 'SymbolsNerdFontMono-Regular.ttf' -d "$dir"
    rm -rf "$tmp"
  fi
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
  # Debian's native-comp Emacs also writes eln-cache there on startup,
  # which is only a compile cache, so stop the service and drop it too.
  if [ -d ~/.emacs.d ] && [ ! -L ~/.emacs.d ]; then
    systemctl --user stop emacs.service 2>/dev/null || true
    rm -rf ~/.emacs.d/eln-cache
  fi
  if [ -d ~/.emacs.d ] && [ ! -L ~/.emacs.d ] && ! rmdir ~/.emacs.d 2>/dev/null; then
    warn "$HOME/.emacs.d exists and is not empty, leaving it alone"
    return 0
  fi
  if [ ! -e ~/.emacs.d ] && [ ! -L ~/.emacs.d ]; then
    ln -s ~/.config/smallwat3r-emacs ~/.emacs.d
    systemctl --user restart emacs.service 2>/dev/null || true
  fi
  # Packages are installed by elpaca on first start, emacs.service is
  # enabled by make stow and does that on next login
}

install_claude_airlock() {
  step 'Installing claude-airlock'
  # Its bin/ is on PATH through environment.d/10-path.conf and .zshenv,
  # not symlinked, claude-airlock finds its Dockerfile relative to itself.
  # The image is built on the first claude-airlock run.
  clone claude-airlock ~/code/claude-airlock
}

install_firefox_wm() {
  step 'Installing firefox-sway'
  clone firefox-sway ~/code/firefox-sway
  # firefox-sway links into the default profile, which only exists once
  # Firefox has been started at least once
  [ -f ~/.mozilla/firefox/profiles.ini ] || \
    [ -f "${XDG_CONFIG_HOME:-$HOME/.config}/mozilla/firefox/profiles.ini" ] || \
    firefox --headless --screenshot /dev/null about:blank >/dev/null 2>&1 || true
  make -C ~/code/firefox-sway install
}

install_qmk() {
  step 'Installing QMK'
  # The toolchains (arm-none-eabi-*, avr-*, dfu-*) come from
  # packages.dnf.txt, the CLI itself is only on PyPI. It installs under
  # ~/.local/lib/python3.X, so re-run this after a Fedora upgrade bumps
  # the system Python or `qmk` dies with "No module named 'qmk_cli'".
  if [ "$DISTRO" = debian ]; then
    # Debian marks the system Python as externally managed (PEP 668)
    pipx install qmk || pipx upgrade qmk
    export PATH="$HOME/.local/bin:$PATH"
  else
    python3 -m pip install --user --upgrade qmk
  fi
  # Clones ~/qmk_firmware, syncs its submodules and runs qmk doctor.
  # Skip it once the clone exists, re-running would reset it. It only
  # warns about the udev rules, so install them ourselves, they let
  # `qmk flash` reach the boards without sudo. Since 0.32.12 the rules
  # live in the qmk/qmk_udev repo, not in the tree: the script fetches
  # the latest release, drops them in /etc/udev/rules.d and reloads udev.
  [ -d ~/qmk_firmware ] || qmk setup -y
  sh ~/qmk_firmware/util/install_udev.sh
}

enable_repos
install_packages
install_fonts
enable_services
stow_dotfiles
install_emacs
install_claude_airlock
install_firefox_wm
install_qmk

step 'Done'
if [ "$DISTRO" = debian ]; then
  cat <<'EOF'
Debian / Raspberry Pi OS:
  - copy the MonacoB font into ~/.local/share/fonts and run fc-cache -f
  - pick "Sway (dotfiles)" at the login screen, or make it the default by
    setting user-session and autologin-session to sway-session in
    /etc/lightdm/lightdm.conf
  - the Fedora-only packages listed at the top of packages.apt.txt and
    the flatpaks Flathub does not build for this architecture are not
    installed

EOF
fi
cat <<'EOF'
Remaining manual steps:
  - log out and back in for the new group memberships to apply
  - yubikey-pam-setup      register the YubiKey for sudo and login
  - tailscale up           join the tailnet
  - git clone https://nas.ts.smallwat3r.com/git/smallwat3r/notes.git ~/notes
                           after the tailnet and a token from `make forgejo-token`
                           in the homelab repo, notes-sync.timer fails until then
  - git -C ~/notes pull    once, paste the token from `pass show git/nas.ts.smallwat3r.com`,
                           git stores it in ~/.config/git/credentials-nas for the timer
  - git clone https://nas.ts.smallwat3r.com/git/smallwat3r/password-store.git ~/.password-store
                           pass entries, needs the gpg key first
  - rbw login              Bitwarden CLI, used by the launcher
  - keybase login
  - WEB_EXT_API_KEY=... WEB_EXT_API_SECRET=... make -C ~/code/firefox-sway install
                           install the Tabs to Windows extension from AMO
EOF
