SHELL=/bin/bash

.PHONY: homebrew stow symlink cask brew python pip node npm taps xcode-cli
.DEFAULT: symlink

all: npm pip cask brew symlink
	@echo '*** -- Everything has been installed --'

homebrew:
ifeq ($(shell command -v brew),)
	@echo '*** Installing Homebrew ...'
	curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh | /bin/bash
	@echo '*** Homebrew has been installed'
endif

stow: homebrew
ifeq ($(shell command -v stow),)
	@echo '*** Installing Stow ...'
	brew install stow
	@echo '*** Stow has been installed'
endif

xcode-cli:
	@xcode-select --install >/dev/null 2>&1 && \
		echo '*** Installing xcode cli tools... Please follow the instructions in the GUI' || \
		exit 0

symlink: stow
	@stow stow -vv -t $(HOME)  # must be run first
	@stow scripts -vv -t /usr/local
	@stow \
		alacritty \
		ctags \
		git \
		htop \
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
	@echo '*** Symlinks all set-up'

taps: homebrew
	@while read -r line; do \
		brew tap "$$line"; \
        done <./brew/taps

brew: taps xcode-cli
	@while read -r line; do \
		echo "*** Checking $$line" && \
		brew ls --versions "$$line" >/dev/null || { \
			echo "*** Installing $$line"; \
			brew install "$$line"; \
		}; \
        done <./brew/brew
	@echo '*** All done checking brew'

cask: taps
	@while read -r line; do \
		echo "*** Checking $$line" && \
		brew cask list "$$line" >/dev/null || { \
			echo "*** Installing cask $$line"; \
			brew cask install "$$line" | true;\
		}; \
        done <./brew/cask
	@echo '*** All done checking casks'

node: homebrew
ifeq ($(shell brew ls --versions node),)
	@echo '*** Installing node ...'
	brew install node
	@echo '*** Node has been installed'
endif

npm: node
	npm install -g \
		prettier \
		prettydiff \
		http-server
	@echo '*** All npm packages installed'

python: homebrew
ifeq ($(shell brew ls --versions python@3.8),)
	@echo '*** Installing python 3.8 ...'
	brew install python@3.8
	ln -s -f $(shell which python3.8) /usr/local/bin/python
	@echo '*** Python 3.8 has been installed'
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
	@echo '*** All pip packages installed'
