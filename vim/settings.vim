" File              : settings.vim
" Author            : smallwat3r
" Date              : Wed  7 Aug 19:49:19 2019

" General settings

syntax on
filetype indent plugin on

" Leader
let mapleader=','

" General
set autoread
set encoding=utf8
set ffs=unix
set fileencoding=utf-8
set listchars=tab:▸\ ,eol:¬,extends:❯,precedes:❮
set showbreak=↪
set ttyfast
set laststatus=2
set modifiable
set showcmd
set showmatch
set mouse=a
set nostartofline
set incsearch
set hlsearch
set clipboard=unnamed
set ruler
set expandtab
set autoindent
set si
set wrap
set showbreak=>\
set lazyredraw
set ignorecase
set scrolljump=8
set autochdir
set linespace=-1
set list
set nonu
set fillchars=vert:┃
set nocompatible

" Deactivate bells
set noerrorbells
set visualbell
set t_vb=
set tm=500

" Indents
set sw=2 ts=2 sts=2
autocmd FileType python :setlocal sw=4 ts=4 sts=4
autocmd FileType py :setlocal sw=4 ts=4 sts=4

" No swp files / backups etc
set noswapfile
set nobackup

" Backspace as it should work
set backspace=indent,eol,start
set whichwrap+=<,>,h,l

" Change cursor format on insert
let &t_SI = "\e[6 q"
let &t_EI = "\e[3 q"

" Netrw
let g:netrw_liststyle=3
let g:netrw_banner=0
let g:netrw_winsize=20
let g:netrw_list_hide= '.*\.swp$,.DS_Store,*/tmp/*,*.so,*.swp,*.zip,*.git,^\.\.\=/\=$'
let g:netrw_bufsettings='noma nomod nonu nowrap ro nobl'

" Full python highlighting
let python_highlight_all=1

" Statusline
set statusline=%F%m%r%h%w\
set statusline+=%{fugitive#statusline()}\
set statusline+=[%{strlen(&fenc)?&fenc:&enc}]
set statusline+=\ [line\ %l\/%L]

" Italics
let &t_ZH="\e[3m"
let &t_ZR="\e[23m"

" MacVim Gui
if (has("gui_running"))
  set termguicolors
  set transparency=5
  set guifont=AndaleMonoSwFix:h13
endif

set t_Co=256
set bg=dark
colo ambient

hi Normal ctermbg=None
