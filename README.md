# Dotfiles  

My config files for macOS.  

**This repo is primarly created for personnal use, and is updated frequently**  
**!!! Please create backups of your current set-up if you intend to use these dotfiles !!!**  

Clone repo
```sh
git clone https://github.com/smallwat3r/dotfiles.git
```

## Commands

Allow user to access man pages (needed to install packages via brew)
```sh
sudo chown -R $(whoami) /usr/local/share/man/man3
```

Install Homebrew
```sh
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
```

Dotfiles are managed by a YAML config file `config.yml`  
It requires Perl `YAML::XS` (scripts `brew` and `symlink`)  
Step to do after installing Homebrew  
```sh
perl -MCPAN -e 'install YAML::XS'
```

Install oh-my-zsh
```sh
sh -c "$(curl -fsSL https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
```

Install brew packages, tools, and casks
```sh
perl brew
```

Install antigen (zsh) (will be symlinked to `~/.antigen/antigen.zsh`)
```sh
curl -L git.io/antigen > ~/dotfiles/zsh/antigen.zsh
```

neovim vim-plug plugin manager
```sh
curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

# Then run :PlugInstall
```

python requirements (mainly for formatting)
```sh
pip3 install -r pip/requirements.txt
```

macOS settings
```sh
sh macos/macos
# Then log user out and back in for all changes to apply
```

Symlink all dotfiles to system
```sh
perl symlink
```

Mononoki Nerd fonts
```sh
wget -O mononoki-regular-nerd.ttf \
    https://github.com/ryanoasis/nerd-fonts/raw/master/patched-fonts/Mononoki/Regular/complete/mononoki-Regular%20Nerd%20Font%20Complete%20Mono.ttf
wget -O mononoki-italic-nerd.ttf \
    https://github.com/ryanoasis/nerd-fonts/raw/master/patched-fonts/Mononoki/Italic/complete/mononoki%20Italic%20Nerd%20Font%20Complete%20Mono.ttf
wget -O mononoki-italic-bold-nerd.ttf \
    https://github.com/ryanoasis/nerd-fonts/raw/master/patched-fonts/Mononoki/Bold-Italic/complete/mononoki%20Bold%20Italic%20Nerd%20Font%20Complete%20Mono.ttf
wget -O mononoki-bold-nerd.ttf \
    https://github.com/ryanoasis/nerd-fonts/raw/master/patched-fonts/Mononoki/Bold/complete/mononoki%20Bold%20Nerd%20Font%20Complete%20Mono.ttf
```
