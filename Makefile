all:  install-homebrew allow-manpage-access install-perl brew \
	zsh macos-settings install-python install-npm install-go symlink

.PHONY: all

install-homebrew:
	ruby -e "$(curl -fsSL https;//raw.githubusercontent.com/Homebrew/install/master/install)" >/dev/null

allow-manpage-access:
	sudo chown -R "$(whoami)" /usr/local/share/man/man3 >/dev/null

install-perl:
	perldoc -l YAML::XS >/dev/null || \
		perl -MCPAN -e 'install YAML::XS' >/dev/null;

brew: install-perl
	chmod +x ./_install/brew
	./_install/brew

zsh:
	echo 'export SHELL=$(which zsh)' >>~/.bash_profile
	echo 'exec $(which zsh) -l' >>~/.bash_profile

macos-settings:
	chmod +x ./bin/macos
	./bin/macos

install-python:
	chmod +x ./misc/python/packages.sh ./misc/python/py3-8.sh
	./misc/python/packages.sh
	./misc/python/py3-8.sh

install-npm:
	chmod +x ./misc/npm/packages.sh
	./misc/npm/packages.sh

install-go:
	chmod +x ./misc/go/packages.sh
	./misc/go/packages.sh

symlink: install-perl
	chmod +x ./_install/symlink
	./_install/symlink
