SHELL = /bin/bash

CURRENT_DIR := $(shell pwd)
FONTS_DIR   := /Library/Fonts

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

.PHONY: install
install: npm pip symlink nvim brew ## * Install everything and symlink
	@echo '$(SUCCESS)*** -- Everything has been installed --$(SGR0)'

.PHONY: symlink
symlink: _stow _localbin _maildir ## * Symlink all the dotfiles using stow
# This instruction must be run first as this is linking the main stow configuration.
	@stow stow --verbose=2 --target "$(HOME)"
# Stow home directory relative configurationss.
	@stow \
		bin \
		emacs \
		git \
		gnupg \
		linters \
		mail \
		ssh \
		terminal \
		utils \
		vim \
		workflows \
		zsh \
		--verbose=2 --restow --target "$(HOME)"
# Stow root directory relative configurations. This might need to run with `sudo`
# as it is targetting the system root directory.
	@stow plist --verbose=2 --target '/'
	@echo ''
	@echo '$(SUCCESS)*** Successfully linked all dotfiles$(SGR0)'

# .PHONY: fonts
# fonts: ## Install fonts
# define register_font
# 	@[ -f $(FONTS_DIR)/$(1).ttf ] || cp $(CURRENT_DIR)/fonts/$(1).ttf $(FONTS_DIR)
# endef
# 	@echo '*** Installing Custom Glyphs Hack fonts'
# 	$(call register_font,Custom-Hack-Regular)
# 	$(call register_font,Custom-Hack-Bold)
# 	$(call register_font,Custom-Hack-Italic)
# 	$(call register_font,Custom-Hack-BoldItalic)

.PHONY: brew
brew: homebrew xcode-cli  ## Install all packages from Brewfile
	@brew update
	@brew bundle

.PHONY: npm
npm: node ## Install npm packages
	@echo '$(INFO)*** Installing npm packages ...$(SGR0)'
	npm install -g \
		prettier \
		prettydiff \
		http-server \
		tree-sitter-cli

.PHONY: pip
pip: python ## Install pip packages
	@echo '$(INFO)*** Installing pip packages ...$(SGR0)'
	pip3 install \
		bandit \
		black \
		font-line \
		isort \
		jedi \
		jsbeautifier \
		pylint \
		pynvim \
		pyflakes \
		pytest \
		sqlparse \
		yapf

.PHONY: python
python: homebrew ## Install Python 3.9
ifeq ($(shell brew ls --versions python@3.9),)
	@echo '$(INFO)*** Installing python 3.9 ...$(SGR0)'
	brew install python@3.9
	ln -s -f $(shell which python3.9) /usr/local/bin/python
endif

.PHONY: node
node: homebrew ## Install Node
ifeq ($(shell brew ls --versions node),)
	@echo '$(INFO)*** Installing node ...$(SGR0)'
	brew install node
endif

.PHONY: homebrew
homebrew: ## Install Homebrew
ifeq ($(shell command -v brew),)
	@echo '$(INFO)*** Installing Homebrew ...$(SGR0)'
	curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh | /bin/bash
endif

.PHONY: nvim
nvim: homebrew  ## Install and setup Neovim
ifeq ($(shell brew ls --versions nvim),)
	@echo '$(INFO)*** Installing neovim ...$(SGR0)'
	brew install nvim
endif
	@nvim +PlugInstall +qall >/dev/null
	@echo '$(SUCCESS)Neovim setup successfully!$(SGR0)'

.PHONY: xcode-cli
xcode-cli: ## Install macOS command line tools
	@xcode-select --install >/dev/null 2>&1 && \
		echo '$(INFO)*** Installing macOS command line tools...$(SGR0)' && \
		echo '$(WARNING)...Please follow the instructions from the GUI...$(SGR0)' || \
		exit 0


# Utils (not showing in help menu)

.PHONY: _maildir
_maildir:
	@mkdir -p ~/Maildir/personal || exit 0
	@mkdir -p ~/Maildir/sws || exit 0

.PHONY: _localbin
_localbin:
	@mkdir -p ~/.local/bin || exit 0

.PHONY: _stow
_stow: homebrew
ifeq ($(shell command -v stow),)
	@echo '$(INFO)*** Installing Stow ...$(SGR0)'
	brew install stow
endif
