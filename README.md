# Dotfiles  

My config files for macOS.  

**This repo is primarly created for personal use, and is updated frequently**  
**!!! Please create backups of your current set-up if you intend to use these dotfiles !!!**  

Clone repo (with submodules)
```sh
git clone --recursive https://github.com/smallwat3r/dotfiles.git
```

## Commands

Install Homebrew
```sh
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
```

Allow user to access man pages (needed to install packages via brew)
```sh
sudo chown -R $(whoami) /usr/local/share/man/man3
```

Dotfiles are managed by a YAML config file `config.yml`  
It requires Perl `YAML::XS` (scripts `brew` and `symlink`)  
Step to do after installing Homebrew  
```sh
perl -MCPAN -e 'install YAML::XS'
```

Install brew packages, tools, and casks
```sh
perl brew
```

Install oh-my-zsh
```sh
sh -c "$(curl -fsSL https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
```

Force Zsh
```
echo 'export SHELL=$(which zsh)' >> ~/.bash_profile && echo 'exec $(which zsh) -l' >> ~/.bash_profile
```

Install antigen (zsh) (will be symlinked to `~/.antigen/antigen.zsh`)
```sh
curl -L git.io/antigen > ~/dotfiles/zsh/antigen.zsh
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
