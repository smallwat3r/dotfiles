.PHONY: help all setup-homebrew macos amethyst python-pack npm-pack go-pack dotfiles
.DEFAULT: help

help:
	@echo "make all"
	@echo "  Install everything"
	@echo "make dotfiles"
	@echo "  Install only tools and symlinks"
	@echo "make macos"
	@echo "  Set macos preferences"
	@echo "make amethyst"
	@echo "  Set amethyst preferences"
	@echo "make python-pack"
	@echo "  Install python packages and make sure default is 3.8"
	@echo "make npm-pack"
	@echo "  Install npm packages"
	@echo "make go-pack"
	@echo "  Install go packages"

all: setup-homebrew dotfiles python-pack npm-pack go-pack macos amethyst

setup-homebrew:
	@./files/bin/homebrew-install

macos:
	@./files/bin/macos

amethyst:
	@./files/bin/amethyst

python-pack:
	@./files/code/python/packages.sh
	@./files/code/python/py3-8.sh

npm-pack:
	@./files/code/npm/packages.sh

go-pack:
	@./files/code/go/packages.sh

dotfiles: setup-homebrew
	@./install.sh
