# Dotfiles  

Clone repository and load submodules needed for vim  
```
git clone --recursive-submodules -j8 https://github.com/smallwat3r/dotfiles.git
```

Symlink vim config to ~/.vimrc  
```
ln -fs ~/dotfiles/vim/vimrc ~/.vimrc
```

Symlink alacritty config to ~/.config/alacritty/alacritty.yml  
```
ln -fs ~/dotfiles/alacritty.yml ~/.config/alacritty/alacritty.yml
```

Symlink rest of the files using:  
```
ln -fs <file> ~/.<file>
```

