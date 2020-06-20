<h3 align="center">.dotfiles</h3>
<p align="center">My config files for macOS</p>

---

Dotfiles are managed from `install.sh`.  

This repo was created for my personal use and is updated very
frequently, but feel free to pick whatever you want.  

Use at your own risks and please create back-ups of your current
set-up if you intend to use the Makefile.  

**Download**  
```sh
git clone --recursive https://github.com/smallwat3r/dotfiles.git
```

**Main commands (from Makefile)**
```sh
make all        # install everything
make dotfiles   # do all symlinks + cask/brew installs
```

Run `make help` to see all commands available.  

**Notes**

Before first time use, if on macos you might not have `make`
installed, run (from repo):  
```sh
./files/bin/homebrew-install && brew install make
```

On first time use, after install, run the below command to make
ZSH the default shell (might ask password for user):  
```sh
chsh -s $(which zsh)
```
