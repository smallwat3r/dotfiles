# Dotfiles  

My config files for macOS.  

**Please create backups of your current files if you are using the script below**   

Require Perl YAML::XS module  
```sh
perl -MCPAN -e 'install YAML::XS'
```

Make files executables  
```sh
chmod +x install
chmod +x do-symlink
```

Install everything with  
```sh
./install
```

Run symlinks only  
```sh
./do-symlink
```
