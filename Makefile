.PHONY: homebrew stow symlink cask brew python pip node npm taps xcode-cli fonts help

SHELL=/bin/bash

help: ## Show this help menu
	@echo "Usage: make [TARGET ...]"
	@echo ""
	@grep --no-filename -E '^[a-zA-Z_%-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "%-10s %s\n", $$1, $$2}'

install: npm pip cask brew fonts symlink ## Installs everything
	@echo '*** -- Everything has been installed --'

symlink: stow ## Symlinks dotfiles using stow
	@stow stow -vv -t $(HOME)  # must be run first
	@stow scripts -vv -t /usr/local
	@stow \
		alacritty \
		ctags \
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
ifeq ($(shell command -v wget),)
	@echo '*** Installing wget ...'
	brew install wget
endif
	@[ -f "/Library/Fonts/Go-Mono.ttf" ] || { \
		wget -O /Library/Fonts/Go-Mono.ttf \
			https://github.com/golang/image/raw/master/font/gofont/ttfs/Go-Mono.ttf; \
	}
	@[ -f "/Library/Fonts/Go-Mono-Italic.ttf" ] || { \
		wget -O /Library/Fonts/Go-Mono-Italic.ttf \
			https://github.com/golang/image/raw/master/font/gofont/ttfs/Go-Mono-Italic.ttf; \
	}
	@[ -f "/Library/Fonts/Go-Mono-Bold.ttf" ] || { \
		wget -O /Library/Fonts/Go-Mono-Bold.ttf \
			https://github.com/golang/image/raw/master/font/gofont/ttfs/Go-Mono-Bold.ttf; \
	}

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
