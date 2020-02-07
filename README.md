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

Clone repo with submodules and make files executable  
```sh
git clone https://github.com/smallwat3r/dotfiles.git
```

Access man pages  
```sh
sudo chown -R $(whoami) /usr/local/share/man/man3
```

zsh antigen  
```sh
curl -L git.io/antigen > ~/dotfiles/zsh/antigen.zsh
```

neovim vim-plug  
```sh
curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
```

## Commands
```sh
sh install   # installs everything

perl symlink   # run symlinks only
perl brew      # run brew & casks only
```
