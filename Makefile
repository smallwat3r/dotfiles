all:  install-homebrew allow-manpage-access install-oh-my-zsh install-perl brew \
	macos-settings install-python install-npm install-go symlink

.PHONY: all

install-homebrew:
	ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

allow-manpage-access:
	sudo chown -R "$(whoami)" /usr/local/share/man/man3

install-oh-my-zsh:
	sh -c "$(curl -fsSL https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
	echo 'export SHELL=$(which zsh)' >>~/.bash_profile
	echo 'exec $(which zsh) -l' >>~/.bash_profile

install-perl:
	perl -MCPAN -e 'install YAML::XS'

brew: install-perl
	chmod +x ./bin/brew
	./bin/brew

macos-settings:
	chmod +x ./bin/macos
	./bin/macos

install-python:
	chmod +x ./misc/python/packages.sh ./python/py3-8.sh
	./misc/python/packages.sh
	./misc/python/py3-8.sh

install-npm:
	chmod +x ./misc/npm/packages.sh
	./misc/npm/packages.sh

install-go:
	chmod +x ./misc/go/packages.sh
	./misc/go/packages.sh

symlink: install-perl
	chmod +x ./bin/symlink
	./bin/symlink
