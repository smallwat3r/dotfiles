.PHONY: help all setup-homebrew macos python-pack npm-pack go-pack dotfiles
.DEFAULT: help

help:
	@echo "make all"
	@echo "  Install everything"
	@echo "make dotfiles"
	@echo "  Install only tools and symlinks"
	@echo "make macos"
	@echo "  Install macos preferences"
	@echo "make python-pack"
	@echo "  Install python packages and make sure default is 3.8"
	@echo "make npm-pack"
	@echo "  Install npm packages"
	@echo "make go-pack"
	@echo "  Install go packages"

all: setup-homebrew dotfiles python-pack npm-pack go-pack macos

setup-homebrew:
	@./files/bin/homebrew-install

macos:
	@./files/bin/macos

python-pack:
	@./files/code/python/packages.sh
	@./files/code/python/py3-8.sh

npm-pack:
	@./files/code/npm/packages.sh

go-pack:
	@./files/code/go/packages.sh

dotfiles: setup-homebrew
	@./install.sh
