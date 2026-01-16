SHELL := /bin/bash

DISTRO := $(shell ./_ops/get_distro)

SUCCESS := $(shell tput setaf 40)
INFO    := $(shell tput setaf 111)
WARNING := $(shell tput setaf 178)
SGR0    := $(shell tput sgr0)

STOW_OPTS := --verbose=1 --restow --target

.PHONY: help symlink _dirs _requirements

help: ## Show this help menu and exit
	@echo "Usage: make [TARGET ...]"
	@echo ""
	@grep --no-filename -E '^[a-zA-Z_%-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "%-15s %s\n", $$1, $$2}'

symlink: _requirements _dirs ## Symlink all the dotfiles using stow
	@rm -f ~/.gnupg/gpg-agent.conf
	@stow _stow $(STOW_OPTS) "$(HOME)"
	@stow cross-platform $(STOW_OPTS) "$(HOME)"
ifeq ($(DISTRO),macos)
	@echo '$(INFO)** Stowing macOS dotfiles$(SGR0)'
	@stow macos $(STOW_OPTS) "$(HOME)"
	@sudo stow macos-root $(STOW_OPTS) '/'
endif
ifeq ($(DISTRO),fedora)
	@echo '$(INFO)** Stowing Fedora dotfiles$(SGR0)'
	@stow fedora $(STOW_OPTS) "$(HOME)"
endif
	@echo ''
	@echo '$(SUCCESS)*** Successfully linked all dotfiles$(SGR0)'

_dirs:
	@mkdir -p ~/.local/bin
	@mkdir -p ~/.ssh/sockets
	@mkdir -p ~/Maildir/personal ~/Maildir/sws

_requirements:
	@stow --version >/dev/null 2>&1 || (echo '$(WARNING)*** Stow is required$(SGR0)'; exit 1)
