.PHONY: homebrew stow symlink cask brew python pip node npm taps xcode-cli fonts help install-doom

SHELL=/bin/bash

CURRENT_DIR=$(shell pwd)
FONTS_DIR=/Library/Fonts

help: ## Show this help menu
	@echo "Usage: make [TARGET ...]"
	@echo ""
	@grep --no-filename -E '^[a-zA-Z_%-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "%-15s %s\n", $$1, $$2}'

install: npm pip cask brew fonts install-doom symlink ## Installs everything
	@echo '*** -- Everything has been installed --'

symlink: stow ## Symlinks dotfiles using stow
	@stow stow -vv -t $(HOME)  # must be run first
	@stow scripts -vv -t /usr/local
	@stow \
		alacritty \
		ctags \
		doom \
		git \
		htop \
		isort \
		kitty \
		mypy \
		nvim \
		pip \
		prettier \
		pylint \
		rg \
		sketch \
		ssh \
		tmux \
		vifm \
		yapf \
		zsh \
		-vv -t $(HOME)
	@echo '*** Symlinks all set-up'

fonts: homebrew ## Install fonts
define register_font
	@[ -f $(FONTS_DIR)/$(1).ttf ] || cp $(CURRENT_DIR)/fonts/$(1).ttf $(FONTS_DIR)
endef
	@echo '*** Installing Anonymous Pro fonts'
	$(call register_font,AnonymousPro-Regular)
	$(call register_font,AnonymousPro-Bold)
	$(call register_font,AnonymousPro-Italic)
	$(call register_font,AnonymousPro-BoldItalic)
	@echo '*** Installing Hack (modified) fonts'
	$(call register_font,Hack-Regular)
	$(call register_font,Hack-Bold)
	$(call register_font,Hack-Italic)
	$(call register_font,Hack-BoldItalic)
	@echo '*** Installing Courier Prime Code fonts'
	$(call register_font,Courier-Prime-Code)
	$(call register_font,Courier-Prime-Code-Italic)

homebrew: ## Make sure homebrew is installed
ifeq ($(shell command -v brew),)
	@echo '*** Installing Homebrew ...'
	curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh | /bin/bash
endif

stow: homebrew ## Make sure stow is installed
ifeq ($(shell command -v stow),)
	@echo '*** Installing Stow ...'
	brew install stow
endif

install-doom: ## Install doom emacs
	git clone --depth 1 'https://github.com/hlissner/doom-emacs' '~/.emacs.d'
	~/.emacs.d/bin/doom install

xcode-cli: ## Install xcode command line tools
	@xcode-select --install >/dev/null 2>&1 && \
		echo '*** Installing xcode cli tools... Please follow the instructions in the GUI' || \
		exit 0

taps: homebrew ## Install brew taps
	@while read -r line; do \
		brew tap "$$line"; \
        done <./brew/taps

brew: taps xcode-cli ## Install brew packages
	@echo '*** Installing brew packages ...'
	@while read -r line; do \
		echo "* Checking $$line" && \
		brew ls --versions "$$line" >/dev/null || { \
			echo "* Installing $$line"; \
			brew install "$$line"; \
		}; \
        done <./brew/brew
	@ln -s /usr/local/opt/emacs-plus/Emacs.app /Applications/Emacs.app

cask: taps ## Install brew casks
	@echo '*** Installing brew casks ...'
	@while read -r line; do \
		echo "* Checking $$line" && \
		brew cask list "$$line" >/dev/null || { \
			echo "* Installing cask $$line"; \
			brew cask install "$$line" | true;\
		}; \
        done <./brew/cask

node: homebrew ## Install node
ifeq ($(shell brew ls --versions node),)
	@echo '*** Installing node ...'
	brew install node
endif

npm: node ## Install npm packages
	@echo '*** Installing npm packages ...'
	npm install -g \
		prettier \
		prettydiff \
		http-server

python: homebrew ## Install python 3.8
ifeq ($(shell brew ls --versions python@3.8),)
	@echo '*** Installing python 3.8 ...'
	brew install python@3.8
	ln -s -f $(shell which python3.8) /usr/local/bin/python
endif

pip: python ## Install pip packages
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
