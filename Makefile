SHELL=/bin/bash

.PHONY: homebrew stow symlink cask brew python pip node npm
.DEFAULT: symlink

all: symlink npm pip cask brew

homebrew:
ifeq ($(shell command -v brew),)
	@echo "[dotfiles] Installing Homebrew ..."
	curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh | /bin/bash
	@echo "[dotfiles] Homebrew has been installed"
endif

stow: homebrew
ifeq ($(shell command -v stow),)
	@echo "[dotfiles] Installing Stow ..."
	brew install stow
	@echo "[dotfiles] Stow has been installed"
endif

symlink: stow
	@stow stow -vv -t $(HOME)  # must be run first
	@stow scripts -vv -t /usr/local
	@stow \
		alacritty \
		git \
		ctags \
		isort \
		mypy \
		nvim \
		pip \
		pylint \
		prettier \
		rg \
		scripts \
		sketch \
		tmux \
		yapf \
		zsh \
		-vv -t $(HOME)
	@echo "[dotfiles] All set-up"

brew: homebrew
	@while read -r line; do \
		echo "[dotfiles] Checking $$line" && \
		brew ls --versions "$$line" >/dev/null || { \
			echo "[dotfiles] Installing $$line"; \
			brew install "$$line"; \
		}; \
        done <./brew/brew

cask: homebrew
	@while read -r line; do \
		echo "[dotfiles] Checking $$line" && \
		brew cask list "$$line" >/dev/null || { \
			echo "[dotfiles] Installing cask $$line"; \
			brew cask install "$$line"; \
		}; \
        done <./brew/cask

node: homebrew
ifeq ($(shell brew ls --versions node),)
	@echo "[dotfiles] Installing node ..."
	brew install node
	@echo "[dotfiles] Node has been installed"
endif

npm: node
	npm install -g \
		prettier \
		prettydiff \
		http-server
	@echo "[dotfiles] All npm packages installed"

python: homebrew
ifeq ($(shell brew ls --versions python@3.8),)
	@echo "[dotfiles] Installing python 3.8 ..."
	brew install python@3.8
	ln -s -f $(shell which python3.8) /usr/local/bin/python
	@echo "[dotfiles] Python 3.8 has been installed"
endif

pip: python
	pip3 install \
		yapf \
		black \
		pylint \
		sqlparse \
		isort \
		jsbeautifier \
		pynvim \
		bandit
	@echo "[dotfiles] All pip packages installed"
