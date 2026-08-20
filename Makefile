SHELL := /bin/bash

DISTRO := $(shell \
    if [[ "$$(uname)" == "Darwin" ]]; then echo macos; \
    elif [[ -f /etc/os-release ]]; then . /etc/os-release && echo $$ID; \
    else echo unknown; fi)

# Platform directory to stow alongside base (empty on unsupported distros)
PLATFORM := $(filter macos fedora,$(DISTRO))

SUCCESS := $(shell tput setaf 40)
INFO    := $(shell tput setaf 111)
WARNING := $(shell tput setaf 178)
SGR0    := $(shell tput sgr0)

STOW_OPTS := --verbose=1 --restow --target

ZSH_FILES := base/.zshenv base/.zprofile base/.zshrc \
    $(wildcard base/.zsh/core/*.zsh base/.zsh/tools/*.zsh base/.zsh/functions/*)

.PHONY: help stow unstow dry-run lint _dirs _requirements

help: ## Show this help menu and exit
	@echo "Usage: make [TARGET ...]"
	@echo ""
	@grep --no-filename -E '^[a-zA-Z_%-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "%-15s %s\n", $$1, $$2}'

stow: _requirements _dirs ## Stow all the dotfiles
	@stow base $(STOW_OPTS) "$(HOME)"
ifneq ($(PLATFORM),)
	@echo '$(INFO)** Stowing $(PLATFORM) dotfiles$(SGR0)'
	@stow $(PLATFORM) --ignore='_root' $(STOW_OPTS) "$(HOME)"
	@sudo stow -d $(PLATFORM) _root $(STOW_OPTS) '/'
endif
	@echo ''
	@echo '$(SUCCESS)*** Successfully linked all dotfiles$(SGR0)'

unstow: _requirements ## Remove all symlinks
	@stow -D base $(STOW_OPTS) "$(HOME)"
ifneq ($(PLATFORM),)
	@echo '$(INFO)** Unstowing $(PLATFORM) dotfiles$(SGR0)'
	@stow -D $(PLATFORM) --ignore='_root' $(STOW_OPTS) "$(HOME)"
	@sudo stow -D -d $(PLATFORM) _root $(STOW_OPTS) '/'
endif
	@echo ''
	@echo '$(SUCCESS)*** Successfully removed all symlinks$(SGR0)'

dry-run: _requirements ## Show what would be linked (no changes made)
	@echo '$(INFO)** Dry run - no changes will be made$(SGR0)'
	@stow -n -v2 --restow --target "$(HOME)" base 2>&1 || true
ifneq ($(PLATFORM),)
	@stow -n -v2 --restow --ignore='_root' --target "$(HOME)" $(PLATFORM) 2>&1 || true
	@stow -n -v2 --restow -d $(PLATFORM) --target '/' _root 2>&1 || true
endif

lint: ## Syntax-check all shell scripts (shellcheck + zsh -n)
	@grep -rlE '^#!.*\b(ba)?sh$$' base fedora macos | xargs shellcheck
	@shellcheck base/.local/lib/launcher.sh
	@printf '%s\0' $(ZSH_FILES) | xargs -0 -n1 zsh -n
	@echo '$(SUCCESS)*** Lint passed$(SGR0)'

_dirs:
	@mkdir -p ~/.local/bin
	@mkdir -p ~/.ssh/sockets

_requirements:
	@stow --version >/dev/null 2>&1 || (echo '$(WARNING)*** Stow is required$(SGR0)'; exit 1)
