# Dotfiles  

Clone repository and load submodules needed for vim  
```
git clone --recursive-submodules -j8 https://github.com/smallwat3r/dotfiles.git
```

## Vim  
Copy vim config to `~/.vim/`  
```
cp -R vim/* ~/.vim
```
Symlink config to `~/.vimrc`
```
ln -fs ~/.vim/vimrc ~/.vimrc
```

## Dotfiles  
Symlink rest of the files using:  
```
ln -fs <file> ~/.<file>
```
