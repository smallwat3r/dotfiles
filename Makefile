SHELL := /bin/bash

DISTRO := $(shell \
    if [[ "$$(uname)" == "Darwin" ]]; then echo macos; \
    elif [[ -f /etc/os-release ]]; then . /etc/os-release && echo $$ID; \
    else echo unknown; fi)

SUCCESS := $(shell tput setaf 40)
INFO    := $(shell tput setaf 111)
WARNING := $(shell tput setaf 178)
SGR0    := $(shell tput sgr0)

STOW_OPTS := --verbose=1 --restow --target

.PHONY: help stow unstow dry-run dconf-load _dirs _requirements

help: ## Show this help menu and exit
	@echo "Usage: make [TARGET ...]"
	@echo ""
	@grep --no-filename -E '^[a-zA-Z_%-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "%-15s %s\n", $$1, $$2}'

stow: _requirements _dirs ## Stow all the dotfiles
	@stow base $(STOW_OPTS) "$(HOME)"
ifeq ($(DISTRO),macos)
	@echo '$(INFO)** Stowing macOS dotfiles$(SGR0)'
	@stow macos --ignore='_root' $(STOW_OPTS) "$(HOME)"
	@sudo stow macos/_root $(STOW_OPTS) '/'
endif
ifeq ($(DISTRO),fedora)
	@echo '$(INFO)** Stowing Fedora dotfiles$(SGR0)'
	@stow fedora $(STOW_OPTS) "$(HOME)"
endif
	@echo ''
	@echo '$(SUCCESS)*** Successfully linked all dotfiles$(SGR0)'

unstow: _requirements ## Remove all symlinks
	@stow -D base $(STOW_OPTS) "$(HOME)"
ifeq ($(DISTRO),macos)
	@echo '$(INFO)** Unstowing macOS dotfiles$(SGR0)'
	@stow -D macos --ignore='_root' $(STOW_OPTS) "$(HOME)"
	@sudo stow -D macos/_root $(STOW_OPTS) '/'
endif
ifeq ($(DISTRO),fedora)
	@echo '$(INFO)** Unstowing Fedora dotfiles$(SGR0)'
	@stow -D fedora $(STOW_OPTS) "$(HOME)"
endif
	@echo ''
	@echo '$(SUCCESS)*** Successfully removed all symlinks$(SGR0)'

dry-run: _requirements ## Show what would be linked (no changes made)
	@echo '$(INFO)** Dry run - no changes will be made$(SGR0)'
	@stow -n -v2 --restow --target "$(HOME)" base 2>&1 || true
ifeq ($(DISTRO),macos)
	@stow -n -v2 --restow --ignore='_root' --target "$(HOME)" macos 2>&1 || true
	@stow -n -v2 --restow --target '/' macos/_root 2>&1 || true
endif
ifeq ($(DISTRO),fedora)
	@stow -n -v2 --restow --target "$(HOME)" fedora 2>&1 || true
endif

DCONF_KEYS_PATH := /org/gnome/settings-daemon/plugins/media-keys
DCONF_KEYS_FILE := fedora/dconf/custom-keybindings.dconf

dconf-load: ## Load dconf settings (Fedora only)
ifneq ($(DISTRO),fedora)
	@echo '$(WARNING)*** dconf-load is only available on Fedora$(SGR0)'
else
	@echo '$(INFO)** Loading dconf settings$(SGR0)'
	@dconf load $(DCONF_KEYS_PATH)/custom-keybindings/ < $(DCONF_KEYS_FILE)
	@dconf write $(DCONF_KEYS_PATH)/custom-keybindings "$$(                     \
		grep -oE '^\[custom[0-9]+\]' $(DCONF_KEYS_FILE)                     \
		| sed "s|\[\(.*\)\]|'$(DCONF_KEYS_PATH)/custom-keybindings/\1/'|"   \
		| paste -sd,                                                        \
		| sed 's/^/[/;s/$$/]/'                                              \
	)"
	@echo '$(SUCCESS)*** dconf settings loaded$(SGR0)'
endif

_dirs:
	@mkdir -p ~/.local/bin
	@mkdir -p ~/.ssh/sockets
	@mkdir -p ~/Maildir/personal ~/Maildir/sws

_requirements:
	@stow --version >/dev/null 2>&1 || (echo '$(WARNING)*** Stow is required$(SGR0)'; exit 1)
