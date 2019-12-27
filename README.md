# Dotfiles  

My config files for macOS.  

**Please create backups of your current files if you are using the script below**   

Require Perl YAML::XS module  
```sh
perl -MCPAN -e 'install YAML::XS'
```

Make files executables  
```sh
chmod +x install symlink brew
```

Install & symlink everything  
```sh
./install
```

Run symlinks only  
```sh
./symlink
```

Install brew packages and cask  
```sh
./brew
```
