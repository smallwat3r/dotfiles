SHELL=/bin/bash

CURRENT_DIR=$(shell pwd)
FONTS_DIR=/Library/Fonts

.PHONY: help
help: ## Show this help menu
	@echo "Usage: make [TARGET ...]"
	@echo ""
	@grep --no-filename -E '^[a-zA-Z_%-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "%-15s %s\n", $$1, $$2}'

.PHONY: install
install: npm pip symlink homebrew-bundle ## Installs everything
	@echo '*** -- Everything has been installed --'

.PHONY: maildir
maildir:
	@mkdir ~/Maildir || exit 0  # make sure Mail directory exists
	@mkdir ~/Maildir/personal || exit 0
	@mkdir ~/Maildir/sws || exit 0

.PHONY: symlink
symlink: stow maildir ## Symlinks dotfiles using stow
	@stow stow -vv -t $(HOME)  # must be run first
	@stow scripts -vv -t /usr/local
	@stow \
		alacritty \
		ctags \
		git \
		gnupg \
		htop \
		isort \
		kitty \
		mbsync \
		msmtp \
		mypy \
		notmuch \
		nvim \
		pip \
		prettier \
		pylint \
		rg \
		sketch \
		ssh \
		tmux \
		vim \
		vifm \
		yapf \
		zsh \
		-vv -t $(HOME)
	@echo '*** Symlinks all set-up'
	@echo '/!\ Note: Doom Emacs configs have not been automatically linked.'
	@echo 'To do so, you can run:'
	@echo '    stow doom -vv -t <home-directory>'

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

.PHONY: homebrew
homebrew: ## Make sure homebrew is installed
ifeq ($(shell command -v brew),)
	@echo '*** Installing Homebrew ...'
	curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh | /bin/bash
endif

.PHONY: stow
stow: homebrew ## Make sure stow is installed
ifeq ($(shell command -v stow),)
	@echo '*** Installing Stow ...'
	brew install stow
endif

.PHONY: xcode-cli
xcode-cli: ## Install xcode command line tools
	@xcode-select --install >/dev/null 2>&1 && \
		echo '*** Installing xcode cli tools... Please follow the instructions from the GUI' || \
		exit 0

.PHONY: brew-bundle
brew-bundle: homebrew xcode-cli  ## Install packages from Brewfile
	@brew update
	@brew bundle

.PHONY: node
node: homebrew ## Install Node
ifeq ($(shell brew ls --versions node),)
	@echo '*** Installing node ...'
	brew install node
endif

.PHONY: npm
npm: node ## Install npm packages
	@echo '*** Installing npm packages ...'
	npm install -g \
		prettier \
		prettydiff \
		http-server

.PHONY: python
python: homebrew ## Install Python 3.9
ifeq ($(shell brew ls --versions python@3.9),)
	@echo '*** Installing python 3.9 ...'
	brew install python@3.9
	ln -s -f $(shell which python3.9) /usr/local/bin/python
endif

.PHONY: pip
pip: python ## Install Python packages
	@echo '*** Installing pip packages ...'
	pip3 install \
		bandit \
		black \
		isort \
		jedi \
		jsbeautifier \
		pylint \
		pynvim \
		sqlparse \
		yapf
