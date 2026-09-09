SHELL := /bin/bash

# Colours only when there is a terminal to show them
TPUT    := $(if $(TERM),tput,true)
SUCCESS := $(shell $(TPUT) setaf 40)
INFO    := $(shell $(TPUT) setaf 111)
WARNING := $(shell $(TPUT) setaf 178)
SGR0    := $(shell $(TPUT) sgr0)

# --no-folding keeps every directory real under the target, so files apps
# write into stowed dirs (ssh keys, systemd wants, zwc) never land in the repo
STOW_OPTS := --verbose=1 --restow --no-folding --target

ZSH_FILES := home/.zshenv home/.zprofile home/.zshrc \
    $(wildcard home/.zsh/core/*.zsh home/.zsh/tools/*.zsh home/.zsh/functions/*)

.PHONY: help bootstrap stow unstow dry-run theme lint _dirs _requirements

help: ## Show this help menu and exit
	@echo "Usage: make [TARGET ...]"
	@echo ""
	@grep --no-filename -E '^[a-zA-Z_%-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "%-15s %s\n", $$1, $$2}'

bootstrap: ## Provision a fresh Fedora install (repos, packages, services, stow)
	@./bootstrap.sh

stow: _requirements _dirs ## Stow all the dotfiles
	@stow home $(STOW_OPTS) "$(HOME)"
	@echo '$(INFO)** Stowing system files to /$(SGR0)'
	@sudo stow root $(STOW_OPTS) '/'
	@echo '$(INFO)** Labelling root/etc as /etc for SELinux$(SGR0)'
	-@sudo semanage fcontext -a -e /etc '$(CURDIR)/root/etc' 2>/dev/null
	@sudo restorecon -R '$(CURDIR)/root/etc'
	@echo '$(INFO)** Enabling user services$(SGR0)'
	@systemctl --user daemon-reload
	@systemctl --user enable emacs.service tailscale-systray.service ssh-agent.socket
	@echo ''
	@echo '$(SUCCESS)*** Successfully linked all dotfiles$(SGR0)'

unstow: _requirements ## Remove all symlinks
	@stow -D home $(STOW_OPTS) "$(HOME)"
	@echo '$(INFO)** Unstowing system files from /$(SGR0)'
	@sudo stow -D root $(STOW_OPTS) '/'
	-@sudo semanage fcontext -d -e /etc '$(CURDIR)/root/etc' 2>/dev/null
	@sudo restorecon -R '$(CURDIR)/root/etc'
	@echo ''
	@echo '$(SUCCESS)*** Successfully removed all symlinks$(SGR0)'

dry-run: _requirements ## Show what would be linked (no changes made)
	@echo '$(INFO)** Dry run - no changes will be made$(SGR0)'
	@stow -n -v2 --restow --no-folding --target "$(HOME)" home 2>&1 || true
	@stow -n -v2 --restow --no-folding --target '/' root 2>&1 || true

theme: ## Regenerate app colour configs from theme/palette
	@sh theme/build.sh
	@echo '$(SUCCESS)*** Theme files regenerated$(SGR0)'

lint: ## Syntax-check all shell scripts (shellcheck + zsh -n)
	@grep -rlE '^#!.*\b(ba)?sh$$' bootstrap.sh home root | xargs shellcheck
	@shellcheck home/.local/lib/*.sh
	@printf '%s\0' $(ZSH_FILES) | xargs -0 -n1 zsh -n
	@echo '$(SUCCESS)*** Lint passed$(SGR0)'

_dirs:
	@install -d -m 700 ~/.ssh ~/.gnupg
	@mkdir -p ~/.local/bin ~/.ssh/sockets ~/.config/Yubico

_requirements:
	@stow --version >/dev/null 2>&1 || (echo '$(WARNING)*** Stow is required$(SGR0)'; exit 1)
