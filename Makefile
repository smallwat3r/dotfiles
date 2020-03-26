all:  install-homebrew allow-manpage-access install-oh-my-zsh install-perl brew \
	macos-settings install-python install-npm install-go install-smallwat3r-bin symlink

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
	chmod +x ./macos/macos
	./macos/macos

install-python:
	chmod +x ./python/packages.sh
	./python/packages.sh

install-npm:
	chmod +x ./npm/packages.sh
	./npm/packages.sh

install-go:
	chmod +x ./go/packages.sh
	./go/packages.sh

install-smallwat3r-bin:
	chmod +x ./bin/install-smallwat3r-scripts
	./bin/install-smallwat3r-scripts

symlink: install-perl
	chmod +x ./bin/symlink
	./bin/symlink
