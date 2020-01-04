# Dotfiles  

My config files for macOS.  

**Please create backups of your current files if you are using the commands below**   

Dotfiles are managed by a YAML config file `config.yml`  

Hence it requires Perl `YAML::XS` module to run 
```sh
perl -MCPAN -e 'install YAML::XS'
```

Clone repo with submodules and make files executables  
```sh
git clone --recursive https://github.com/smallwat3r/dotfiles.git && \
    cd dotfiles && \
    chmod +x install symlink brew
```

**Install & symlink everything**  
```sh
./install
```

_Run symlinks only_  
```sh
./symlink
```

_Install brew packages and cask only_  
```sh
./brew
```
