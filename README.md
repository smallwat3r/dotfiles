# Dotfiles  

My config files for macOS.  

**Please create backups of your current files if you are using any of the commands below**   

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
