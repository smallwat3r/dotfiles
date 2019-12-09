" File              : settings.vim
" Author            : smallwat3r
" Date              : Wed  7 Aug 19:49:19 2019

" General settings

syntax on
filetype plugin indent on

" Indents
set tabstop=2
set shiftwidth=2
set softtabstop=2
set expandtab
set autoindent
set smartindent
autocmd FileType python set tabstop=4 shiftwidth=4 softtabstop=4
autocmd FileType make set tabstop=8 shiftwidth=8 softtabstop=0 noexpandtab

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
set wrap
set showbreak=>\
set lazyredraw
set ignorecase
set scrolljump=8
set autochdir
set linespace=0
set list
set nonu
set fillchars=vert:┃
set nocompatible
set foldmethod=indent
set foldlevel=99

" Deactivate bells
set noerrorbells
set visualbell
set t_vb=
set tm=500

" No swp files / backups etc
set noswapfile
set nobackup

" Backspace as it should work
set backspace=indent,eol,start
set whichwrap+=<,>,h,l

" Change cursor format on insert
" let &t_SI = "\e[6 q"
" let &t_EI = "\e[3 q"

" Ignore files
set wildignore=*.pyc,*.swp,.git/,*.DS_Store
let g:netrw_list_hide='.*\.swp$,.*\.pyc$,.git/,__pycache__/,.*\.DS_Store$'

" Netrw
let g:netrw_liststyle=3
let g:netrw_banner=0
let g:netrw_winsize=20
let g:netrw_bufsettings='noma nomod nonu nowrap ro nobl'

" Full python highlighting
let python_highlight_all=1

" Keep visual selection when reindenting
xnoremap > >gv
xnoremap < <gv

" Save as root
command! WW :w !sudo tee % >/dev/null

" Disable automatic insertion of comment markers
set fo=cjql
autocmd FileType * setl fo-=o fo-=r
autocmd FileType gitcommit setl fo=cjql com+=n:>

set t_Co=256
set bg=dark
colo efficient

" Gui settings
if (has("gui_running"))
  set guifont=sourcecodepro:h13
  " set transparency=5
  set guioptions-=mTrL  " remove all GUI widgets
  set gcr=a:blinkon0    " no blinking cursor
endif

" All the below must be set after colorschemes
" --------------------------------------------
" Statusline
set statusline=%{fugitive#statusline()}
set statusline+=\ %f
set statusline+=\ %l:%c,\%L
set statusline+=\ %{strlen(&ft)?&ft:'none'}
set statusline+=\ %{LinterStatus()}
set statusline+=\ %{strlen(&fenc)?&fenc:&enc}

" Italics
let &t_ZH="\e[3m"
let &t_ZR="\e[23m"
