SHELL = /bin/bash

CURRENT_DIR := $(shell pwd)
IS_GPD      := $(shell ./_ops/is_gpd && echo 'true' || echo 'false')
DISTRO      := $(shell ./_ops/get_distro)

SUCCESS := $(shell tput setaf 40)
INFO    := $(shell tput setaf 111)
WARNING := $(shell tput setaf 178)
SGR0    := $(shell tput sgr0)

.PHONY: help
help: ## Show this help menu and exit
	@echo "Usage: make [TARGET ...]"
	@echo ""
	@grep --no-filename -E '^[a-zA-Z_%-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "%-15s %s\n", $$1, $$2}'

.PHONY: symlink
symlink: _requirements _localbin _maildir ## Symlink all the dotfiles using stow
# this needs to be overwritten every time
	@rm ~/.gnupg/gpg-agent.conf || true
# this instruction must be run first as this is linking the main stow configuration.
	@stow _stow --verbose=1 --restow --target "$(HOME)"
# stow cross platform dotfiles.
	@stow cross-platform --verbose=1 --restow --target "$(HOME)"
# stow macOS dotfiles
ifeq ($(DISTRO), macos)
	@echo '$(INFO)** Stowing macOS dotfiles$(SGR0)'
	@stow macos --verbose=1 --restow --target "$(HOME)"
	@sudo stow macos-root --verbose=1 --restow --target '/'
endif
# stow Linux Fedora dotfiles
ifeq ($(DISTRO), fedora)
	@echo '$(INFO)** Stowing Fedora dotfiles$(SGR0)'
	@stow fedora --verbose=1 --restow --target "$(HOME)"
endif
	@echo ''
	@echo '$(SUCCESS)*** Successfully linked all dotfiles$(SGR0)'

.PHONY: _maildir
_maildir:
	@mkdir -p ~/Maildir/personal || true
	@mkdir -p ~/Maildir/sws || true

.PHONY: _localbin
_localbin:
	@mkdir -p ~/.local/bin || true

.PHONY: _requirements
_requirements:
	@stow --version >/dev/null 2>&1 || (echo '$(WARNING)*** Stow is required$(SGR0)'; exit 1)
