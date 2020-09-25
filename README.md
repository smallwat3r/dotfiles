<h3 align="center">.dotfiles</h3>

This repo is created for my personal use and is updated very
frequently, but feel free to pick whatever you want.
Use at your own risks and please create back-ups of your current
set-up if you intend to use the Makefile.

Be aware these dotfiles are primary intended to be used on macOS.

Dotfiles are managed by **GNU Stow** (will be automatically installed
or run `brew install stow`).
Note: you will need to install **GNU Make** to use the Makefile
(`brew install make`).

Clone this repo into your `$HOME` directory.
```sh
git clone https://github.com/smallwat3r/dotfiles.git $HOME
```

**Commands (from Makefile)**

```console
% make help
Usage: make [TARGET ...]

help       Show this help menu
install    Installs everything
symlink    Symlinks dotfiles using stow
fonts      Install fonts
homebrew   Make sure homebrew is installed
stow       Make sure stow is installed
xcode-cli  Install xcode command line tools
taps       Install brew taps
brew       Install brew packages
cask       Install brew casks
node       Install node
npm        Install npm packages
python     Install python 3.8
pip        Install pip packages
```
