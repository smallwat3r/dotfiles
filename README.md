# Dotfiles  

My config files for macOS.  

**This repo is updated super frequently**  

**!!! Please create backups of your current set-up if you intend to use these dotfiles !!!**  

Note: my `vim/vimrc` is not maintained anymore as I switched to neovim (see `vim/init.vim`)  

Dotfiles are managed by a YAML config file `config.yml`  
Hence it requires Perl `YAML::XS` 
```sh
perl -MCPAN -e 'install YAML::XS'
```

Clone repo
```sh
git clone https://github.com/smallwat3r/dotfiles.git
```

## Commands

Access man pages  
```sh
sudo chown -R $(whoami) /usr/local/share/man/man3
```

Homebrew
```sh
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
```

Oh My Zsh
```sh
sh -c "$(curl -fsSL https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
```

Antigen (zsh) 
```sh
curl -L git.io/antigen > ~/dotfiles/zsh/antigen.zsh
```

neovim vim-plug  
```sh
curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
```

python pip requirements
```sh
pip3 install -r pip/requirements.txt
```

macOS settings
```sh
sh macos/macos
```

Install brew tools
```sh
perl brew
```

Symlink dotfiles
```sh
perl symlink
```
