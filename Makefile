all: setup-homebrew dotfiles python-pack npm-pack go-pack macos
.PHONY: all

setup-homebrew:
	./bin/homebrew-install

macos:
	./bin/macos

python-pack:
	./misc/python/packages.sh
	./misc/python/py3-8.sh

npm-pack:
	./misc/npm/packages.sh

go-pack:
	./misc/go/packages.sh

dotfiles: setup-homebrew
	./install
