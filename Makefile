all: setup-homebrew dotfiles zsh-default python-pack npm-pack go-pack macos
.PHONY: all

setup-homebrew:
	./bin/homebrew-install

zsh-default:
	echo 'export SHELL=$(which zsh)' >>~/.bash_profile
	echo 'exec $(which zsh) -l' >>~/.bash_profile

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
