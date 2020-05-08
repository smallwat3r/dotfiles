<h3 align="center">.dotfiles</h3>
<p align="center">My config files for macOS</p>

---

Screenshot from 08/05/20
![dotfiles](https://i.imgur.com/Cw2pObi.png)

Dotfiles are managed from the `install` script.  

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
make            # install everything
make dotfiles   # do all symlinks + cask/brew installs
```

**Notes**

Before first time use, if on macos you might not have `make`
installed, run (from repo):  
```sh
./bin/homebrew-install && brew install make
```

On first time use, after install, run the below command to make
ZSH the default shell (might ask password for user):  
```sh
chsh -s $(which zsh)
```
