<h3 align="center">.dotfiles</h3>
<p align="center">My configs for macOS</p>

---

This repo is created for my personal use and is updated very
frequently, but feel free to pick whatever you want.  
Use at your own risks and please create back-ups of your current
set-up if you intend to use the Makefile.  

Dotfiles are managed by **GNU Stow** (will be automatically installed
or run `brew install stow`).  
Note: you will need to install **GNU Make** to use the Makefile (`brew
install make`).  

Clone this repo into your `$HOME` directory.  
```sh
git clone https://github.com/smallwat3r/dotfiles.git $HOME
```

**Commands (from Makefile)**

```sh
make all        # run everything (symlink + other)
make symlink    # run GNU Stow to symlink all the dotfiles

# Other:
make brew       # install all brew utils
make cask       # install all brew casks
make npm        # install global node packages
make pip        # install global python packages
```
