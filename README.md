<h3 align="center">.dotfiles</h3>
<p align="center">My configs for macOS</p>

---

This repo was created for my personal use and is updated very
frequently, but feel free to pick whatever you want.  
Use at your own risks and please create back-ups of your current
set-up if you intend to use the Makefile.  

Dotfiles are managed by **GNU Stow** (will be automatically installed).  
Note: you will need to install **GNU Make** to use the Makefile (`brew
install make`).  

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
