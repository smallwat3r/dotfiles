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

" No swp files / backups etc
set noswapfile
set nobackup

" Backspace as it should work
set backspace=indent,eol,start
set whichwrap+=<,>,h,l

" Change cursor format on insert
let &t_SI = "\e[6 q"
let &t_EI = "\e[3 q"

" Ignore files
set wildignore=*.o,*.a,*.so,*.pyc,*.swp,.git/,*.class,*.DS_Store

" Netrw
let g:netrw_liststyle=3
let g:netrw_banner=0
let g:netrw_winsize=20
let g:netrw_bufsettings='noma nomod nonu nowrap ro nobl'

" Full python highlighting
let python_highlight_all=1

set t_Co=256
colo molokai-sw

" Statusline (must be after colorscheme)
hi User1 ctermfg=199   guifg=#ff00af
hi User2 ctermfg=190   guifg=#d7ff00
hi User3 ctermfg=193   guifg=#d7ffaf
set statusline=%1*%{fugitive#statusline()}
set statusline+=\ %3*[
set statusline+=%2*%f%3*
set statusline+=\ %l:%c,\%L
set statusline+=\ %{strlen(&ft)?&ft:'none'}
set statusline+=\ %{LinterStatus()}
set statusline+=\ %{strlen(&fenc)?&fenc:&enc}]

" Italics
let &t_ZH="\e[3m"
let &t_ZR="\e[23m"

" MacVim Gui
if (has("gui_running"))
  set guifont=DroidSansMonoSW:h13
endif

