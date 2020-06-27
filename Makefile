SHELL=/bin/bash

.PHONY: homebrew stow symlink cask brew python pip node npm taps
.DEFAULT: symlink

all: npm pip cask brew symlink
	@echo "[dotfiles] -- Everything has been installed --"

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
		ctags \
		git \
		isort \
		mypy \
		nvim \
		pip \
		prettier \
		pylint \
		rg \
		scripts \
		sketch \
		ssh \
		tmux \
		vifm \
		yapf \
		zsh \
		-vv -t $(HOME)
	@echo "[dotfiles] All set-up"

taps: homebrew
	@while read -r line; do \
		brew tap "$$line"; \
        done <./brew/taps

brew: taps
	@while read -r line; do \
		echo "[dotfiles] Checking $$line" && \
		brew ls --versions "$$line" >/dev/null || { \
			echo "[dotfiles] Installing $$line"; \
			brew install "$$line"; \
		}; \
        done <./brew/brew
	@echo "[dotfiles] All done checking brew"

cask: taps
	@while read -r line; do \
		echo "[dotfiles] Checking $$line" && \
		brew cask list "$$line" >/dev/null || { \
			echo "[dotfiles] Installing cask $$line"; \
			brew cask install "$$line" | true;\
		}; \
        done <./brew/cask
	@echo "[dotfiles] All done checking casks"

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
