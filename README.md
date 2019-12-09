# Dotfiles  

**If you use this repository, please back up your dotfiles before.**  

Clone repository and load submodules needed for vim  

```
git clone --recursive-submodules -j8 https://github.com/smallwat3r/dotfiles.git
```

## Use  

### Vim  

I'm using the [vim-plug](https://github.com/junegunn/vim-plug) plugin manager.  
Install with:  

```sh
curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
```

Then symlink the vim config.  
```
ln -fs ~/dotfiles/vimrc ~/.vimrc
```
And in vim install the plugins:  
```
:PlugInstall
```

### Other dotfiles  

Symlink using:  
```
ln -fs <fullpath>/<name> <destination-path>/.<name>
```

Ex.  
```
ln -fs ~/dotfiles/zshrc ~/.zshrc
```

## Preview  
02/12/19  
![Alt Text](https://github.com/smallwat3r/dotfiles/blob/master/_screenshot/s1.png)  
