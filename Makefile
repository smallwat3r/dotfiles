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
	@./bin/homebrew-install

macos:
	@./bin/macos

python-pack:
	@./code/python/packages.sh
	@./code/python/py3-8.sh

npm-pack:
	@./code/npm/packages.sh

go-pack:
	@./code/go/packages.sh

dotfiles: setup-homebrew
	@./install.sh
