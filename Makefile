all: setup-homebrew dotfiles python-pack npm-pack go-pack macos
.PHONY: all

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
