# Dotfiles  

**If you use this repository, please back up your dotfiles before.**  

Clone repository and load submodules needed for vim  
```
git clone --recursive-submodules -j8 https://github.com/smallwat3r/dotfiles.git
```

## Vim  
Copy vim config to `~/.vim/` & Symlink config to `~/.vimrc`
```
cp -R vim/* ~/.vim && ln -fs ~/.vim/vimrc ~/.vimrc
```

## Dotfiles  
Symlink using:  
```
ln -fs <fullpath>/<name> <destination-path>/.<name>
```

ie.  
```
ln -fs ~/dotfiles/zshrc ~/.zshrc
```

## Preview  
02.10.19  
![Alt Text](https://github.com/smallwat3r/dotfiles/blob/master/_screenshot/preview.png)  
