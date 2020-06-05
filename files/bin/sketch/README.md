<h3 align="center">sketch</h3>
<p align="center">Create and edit draft files on the go from your terminal in your favourite text editor.</p>

---

I always kept a blank file open in a corner of my screen in case I 
needed to quickly note or copy something for later, so I wrote sketch
to help me save time to create or open my notes on the go without
having to leave my terminal.

Sketch works with all text editor that can be triggered from a 
terminal command (vim, nvim, macvim, nano, subl etc...).

![sketch](https://i.imgur.com/uHwSDXJ.gif)  

## Installation
Clone this repository and run the following commands (might need to 
run with sudo).  

```sh
git clone https://github.com/smallwat3r/sketch.git \
  && cd sketch \
  && make install
```

or  
```sh
git clone https://github.com/smallwat3r/sketch.git \
  && cd sketch \
  && cp sketch /usr/local/bin/sketch \
  && chmod 755 /usr/local/bin/sketch
```

or (without cloning)  
```sh
wget https://raw.githubusercontent.com/smallwat3r/sketch/master/sketch \
    -P /usr/local/bin && chmod 755 /usr/local/bin/sketch
```

## Usage

```console
$ sketch --usage
Usage:
  sketch            creates a new draft file.

  sketch <number>   open archived saved file. Ex:
                    sketch 1 will reopen the last saved file.
                    sketch 2 will reopen the previous one
                    etc...

  sketch ls         list all archived files.

  sketch purge      delete all the archived files.

Options:
  -h, --help        show help and exit.
  -u, --usage       show usage and exit.
  -v, --version     script version.
```

## Customization

#### .sketchrc file

You can create a sketchrc file to configure sketch.

```sh
# .sketchrc file directory location by priority order.
$XDG_CONFIG_HOME/.sketchrc
$HOME/.config/.sketchrc
$HOME/.sketchrc
```

* **SKETCH_EDITOR**: sketch default editor, if not set, check for 
`$EDITOR` if global env vars, if not set, default to vim.
* **SKETCH_ARCHIVES**: archived files folder from where your sketch
files will be archived. 

```sh
# .sketchrc example file.

# Your favourite editor to use Sketch (vim, nvim, subl, emacs, nano etc)
SKETCH_EDITOR=vim

# Archives folder.
# Default locations:
#     - $XDG_DATA_HOME/sketch_archives
#     - $HOME/.local/share/sketch_archives 
SKETCH_ARCHIVES=$HOME/.sketch_archives 
```

#### Archived files

Each time a sketch file is created and saved, it is archived under a
specific directory.
```sh
# Sketch file directory location by priority order (if $SKETCH_ARCHIVES not set).
$XDG_DATA_HOME/sketch_archives
$HOME/.local/share/sketch_archives 
```
Archived files naming convention: `<int>.sketch`.  
