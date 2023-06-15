SHELL = /bin/bash

CURRENT_DIR := $(shell pwd)
OS          := $(shell uname)
IS_GPD      := $(shell ./_ops/is_gpd && echo 'true' || echo 'false')

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
# This instruction must be run first as this is linking the main stow configuration.
	@stow _stow --verbose=1 --restow --target "$(HOME)"
# Stow cross platform dotfiles.
	@stow cross-platform --verbose=1 --restow --target "$(HOME)"
# Stow macOS dotfiles
ifeq ($(OS), Darwin)
	@stow macos --verbose=1 --restow --target "$(HOME)"
	@sudo stow macos-root --verbose=1 --restow --target '/'
endif
# Stow Linux Arch dotfiles
ifeq ($(OS), Linux)
# If Arch is running on a GPD pocket, we might need to apply some specific patches.
ifeq ($(IS_GPD), true)
	@echo 'yes!'
endif
	@stow linux-gpd --verbose=1 --restow --target "$(HOME)"
	@sudo stow linux-gpd-root --verbose=1 --restow --target '/'
endif
	@echo ''
	@echo '$(SUCCESS)*** Successfully linked all dotfiles$(SGR0)'

.PHONY: _maildir
_maildir:
	@mkdir -p ~/Maildir/personal || exit 0
	@mkdir -p ~/Maildir/sws || exit 0

.PHONY: _localbin
_localbin:
	@mkdir -p ~/.local/bin || exit 0

.PHONY: _requirements
_requirements:
	@stow --version >/dev/null 2>&1 || (echo '$(WARNING)*** Stow is required$(SGR0)'; exit 1)
