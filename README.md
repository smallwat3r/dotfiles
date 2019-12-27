# Dotfiles  

My config files for macOS.  

**Please create backups of your current files if you are using the scripts below**   

Managed by a YAML config file `config.yml`  

Hence it requires Perl `YAML::XS` module to run 
```sh
perl -MCPAN -e 'install YAML::XS'
```

Make files executables  
```sh
chmod +x install symlink brew
```

**Install & symlink everything**  
```sh
./install
```

__Run symlinks only__  
```sh
./symlink
```

__Install brew packages and cask only__  
```sh
./brew
```
