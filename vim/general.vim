" File  : general.vim
" Author: Matthieu Petiteau <mpetiteau.pro@gmail.com>
" Date  : 04.01.2020

" ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" General configs
" ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

syntax on
filetype plugin indent on

" Remap leader
let mapleader=','

" Default indentation
set expandtab
set autoindent
set smartindent
set ts=4
set sw=4
set sts=4

" Indentation other filetypes
autocmd FileType sql    setlocal ts=2 sw=2 sts=2
autocmd FileType json   setlocal ts=2 sw=2 sts=2
autocmd FileType xml    setlocal ts=2 sw=2 sts=2
autocmd FileType css    setlocal ts=2 sw=2 sts=2
autocmd FileType js     setlocal ts=2 sw=2 sts=2
autocmd FileType html   setlocal ts=2 sw=2 sts=2
autocmd FileType make   setlocal ts=8 sw=8 sts=0 noexpandtab
autocmd FileType go     setlocal ts=8 sw=8 sts=0 noexpandtab

" General
set autoread  " reread changed files automatically
set encoding=utf8
set ffs=unix
set ttyfast
set laststatus=2  " always show statusline
set modifiable
set showmatch  " matching brackets
set mouse=a  " mouse support
set nostartofline
set incsearch  " search pattern
set hlsearch  " search highlighting
set clipboard=unnamed
set wrap  " wrap lines
set lazyredraw  " no redraw
set ignorecase
set scrolljump=8  " minimal nb of lines to scroll when cursor gets off the screen
set autochdir  " auto change working directory
set list  " show additional characters eol
set nonu  " deactivate row numbers
set fillchars=vert:┃
set nocompatible
set showmode  " show vim mode (insert, visual, replace)

" Special chars
set showbreak=↪  " wrap lines symbol
set listchars=eol:¬,extends:>,precedes:<

" Folding
set foldmethod=indent
set foldlevel=99

" Ignore files and folders
set wildignore=*.pyc,*.swp,*.DS_Store,*.rdb
set wildignore+=.git/,__pycache__/,venv/,sdist/

" Deactivate bells and alerts
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

" Netrw settings
let g:netrw_liststyle=3
let g:netrw_banner=0
let g:netrw_bufsettings='noma nomod nonu nowrap ro nobl'
let g:netrw_sort_sequence='[\/]$,*'
let g:netrw_localrmdir="rm -r"
autocmd FileType netrw setlocal bufhidden=delete " Netrw buffer

" Deactivate dbtext plugin error msg on sql completion
let g:loaded_sql_completion=0
let g:omni_sql_no_default_maps=1

" Keep visual selection when reindenting
xnoremap > >gv
xnoremap < <gv

" Save as root
command! WW :w !sudo tee % >/dev/null

" Disable automatic insertion of comment markers
set fo=cjql
autocmd FileType *         setl fo-=o fo-=r
autocmd FileType gitcommit setl fo=cjql com+=n:>
