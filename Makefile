SHELL=/bin/bash

CURRENT_DIR=$(shell pwd)
FONTS_DIR=/Library/Fonts

.PHONY: help
help: ## Show this help menu and exit
	@echo "Usage: make [TARGET ...]"
	@echo ""
	@grep --no-filename -E '^[a-zA-Z_%-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "%-15s %s\n", $$1, $$2}'

.PHONY: install
install: npm pip symlink nvim brew ## * Install everything and symlink
	@echo '*** -- Everything has been installed --'

.PHONY: symlink
symlink: stow localbin maildir ## * Symlink all the dotfiles using stow
	@stow stow -vv -t $(HOME)  # must be run first, symlink the stow config file
	@stow \
		alacritty \
		bin \
		chemacs \
		emacs \
		git \
		gnupg \
		linters \
		mail \
		ssh \
		tmux \
		vim \
		zsh \
		-vv -t $(HOME)
	@echo ''
	@echo '*** Successfully linked all dotfiles'

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
	@echo '*** Installing npm packages ...'
	npm install -g \
		prettier \
		prettydiff \
		http-server \
		tree-sitter-cli

.PHONY: pip
pip: python ## Install pip packages
	@echo '*** Installing pip packages ...'
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
	@echo '*** Installing python 3.9 ...'
	brew install python@3.9
	ln -s -f $(shell which python3.9) /usr/local/bin/python
endif

.PHONY: node
node: homebrew ## Install Node
ifeq ($(shell brew ls --versions node),)
	@echo '*** Installing node ...'
	brew install node
endif

.PHONY: homebrew
homebrew: ## Install Homebrew
ifeq ($(shell command -v brew),)
	@echo '*** Installing Homebrew ...'
	curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh | /bin/bash
endif

.PHONY: nvim
nvim: homebrew  ## Install and setup Neovim
ifeq ($(shell brew ls --versions nvim),)
	@echo '*** Installing neovim ...'
	brew install nvim
endif
	@nvim +PlugInstall +qall >/dev/null
	@echo "Neovim setup successfully"

.PHONY: xcode-cli
xcode-cli: ## Install macOS command line tools
	@xcode-select --install >/dev/null 2>&1 && \
		echo '*** Installing macOS command line tools... Please follow the instructions from the GUI' || \
		exit 0

#
## Utils formulas (not showing in help menu)

.PHONY: maildir
maildir:
	@mkdir -p ~/Maildir/personal || exit 0
	@mkdir -p ~/Maildir/sws || exit 0

.PHONY: localbin
localbin:
	@mkdir -p ~/.local/bin || exit 0

.PHONY: stow
stow: homebrew
ifeq ($(shell command -v stow),)
	@echo '*** Installing Stow ...'
	brew install stow
endif
