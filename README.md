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
git clone --recursive https://github.com/smallwat3r/dotfiles.git && \
    cd dotfiles && \
    chmod +x install symlink brew
```

## Commands
```sh
./install   # installs everything

./symlink   # run symlinks only
./brew      # run brew & casks only
```

## Screen (as of 5/2/2020)

![configscreenshot](https://i.imgur.com/E7dpB6c.png)
