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

**Commands**  
```sh
./install   # installs everything

./symlink   # run symlinks only
./brew      # run brew & casks only
```

------
**vim/vimrc_server**   
This is my actual vim config but setup for remote server (no plugins and GUI)  
```sh
wget -N https://raw.githubusercontent.com/smallwat3r/dotfiles/master/vim/vimrc_server \
     -O ~/.vimrc
```
