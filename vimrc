" File              : vimrc
" Author            : smallwat3r
" Date              : Wed  7 Aug 19:49:35 2019
"
" Vim config.
"

" ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" Plugins
" ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

call plug#begin()

Plug 'Glench/Vim-Jinja2-Syntax'
Plug 'dense-analysis/ale'
Plug 'jiangmiao/auto-pairs'
Plug 'smallwat3r/vim-efficient'
Plug 'junegunn/fzf.vim'
Plug 'chr4/nginx.vim'
Plug 'Chiel92/vim-autoformat'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
Plug 'alpertuna/vim-header'
Plug 'maksimr/vim-jsbeautify'
Plug 'google/vim-jsonnet'
Plug 'plasticboy/vim-markdown'
Plug 'Vimjas/vim-python-pep8-indent'
Plug 'tpope/vim-vinegar'
Plug 'dhruvasagar/vim-table-mode'
Plug 'tpope/vim-surround'
Plug 'othree/html5.vim'
Plug 'alvan/vim-closetag'

call plug#end()

" ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" Plugins configs
" ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

" vim git-gutter
let g:gitgutter_sign_added='+'
let g:gitgutter_sign_modified='~'
let g:gitgutter_sign_removed='-'

" FZF
set rtp+=/usr/local/opt/fzf
let g:fzf_layout={ 'down': '~20%' }
let g:fzf_action={
      \ 'ctrl-t': 'tab split',
      \ 'ctrl-x': 'split',
      \ 'ctrl-v': 'vsplit' }
let g:fzf_colors={
      \ 'fg':      ['fg', 'Normal'],
      \ 'bg':      ['bg', 'Normal'],
      \ 'hl':      ['fg', 'SpecialKey'],
      \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
      \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
      \ 'hl+':     ['fg', 'SpecialKey'],
      \ 'info':    ['fg', 'PreProc'],
      \ 'border':  ['fg', 'Ignore'],
      \ 'prompt':  ['fg', 'Conditional'],
      \ 'pointer': ['fg', 'Exception'],
      \ 'marker':  ['fg', 'Keyword'],
      \ 'spinner': ['fg', 'Label'],
      \ 'header':  ['fg', 'Comment'] }

" Auto Headers on F5
let g:header_auto_add_header=0
let g:header_field_author='Matthieu Petiteau'
let g:header_field_author_email='mpetiteau.pro@gmail.com'
let g:header_field_timestamp_format='%d.%m.%Y'
let g:header_field_modified_timestamp=0
let g:header_field_modified_by=0
map <F5> :AddHeader<CR>

" Ale
let g:ale_echo_msg_error_str = 'E'
let g:ale_echo_msg_warning_str = 'W'
let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'
let g:ale_pattern_options = {
      \ '\.min\.js$': {'ale_linters': [], 'ale_fixers': []},
      \ '\.min\.css$': {'ale_linters': [], 'ale_fixers': []},
      \ }

" Markdown
let g:vim_markdown_folding_disabled=1

" ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" General configs
" ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

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
set showmode
set foldmethod=indent
set foldlevel=99
set wildignore=*.pyc,*.swp,*.DS_Store,dump.rdb,.git/,__pycache__/,venv/,sdist/

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
colo efficient-lean

" Gui settings
if (has("gui_running"))
  set guifont=Hack:h12
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

" ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" Mappings
" ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

" Navigate window panels
nmap <C-h> <C-w>h
nmap <C-j> <C-w>j
nmap <C-k> <C-w>k
nmap <C-l> <C-w>l

" Navigate brackets
map <tab> %

" Go start or end of line
nmap H ^
nmap L $
vmap L g_

" Copy paragraph
nmap cp yap<S-}>p

" Open netrw
map <F6> :Vex<cr>

" Remove search highlight
nmap <leader><space> :nohlsearch<cr>

" Editing and reloading of config
map <leader>e :e! ~/dotfiles/vimrc<cr>
autocmd! bufwritepost ~/dotfiles/vimrc source ~/dotfiles/vimrc

" ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" Functions
" ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

" Remove trailing whitespaces.
fun! StripTrailingWhitespaces()
  if exists('b:noStripWhitespace')
    return
  endif
  let _s=@/
  let l=line(".")
  let c=col(".")
  %s/\s\+$//e
  let @/=_s
  call cursor(l, c)
endfun
autocmd BufWritePre * :call StripTrailingWhitespaces()
autocmd FileType markdown let b:noStripWhitespace=1

" Count errors in status bar.
fun! LinterStatus() abort
  let l:counts = ale#statusline#Count(bufnr(''))
  let l:all_errors = l:counts.error + l:counts.style_error
  let l:all_non_errors = l:counts.total - l:all_errors
  return l:counts.total == 0 ? 'OK' : printf(
        \   '%dW %dE',
        \   all_non_errors,
        \   all_errors
        \)
endfun

" Folds
fun! NeatFoldText()
  let line = ' ' . substitute(getline(v:foldstart), '^\s*"\?\s*\|\s*"\?\s*{{' . '{\d*\s*', '', 'g') . ' '
  let lines_count = v:foldend - v:foldstart + 1
  let lines_count_text = printf("%10s", '(' . lines_count . ')') . ' .'
  let foldchar = matchstr(&fillchars, 'fold:\zs.')
  let foldtextend = strpart(repeat(foldchar, v:foldlevel*2) . line, 0, (winwidth(0)*2)/3)
  let foldtextstart = '+ ' . lines_count_text . repeat(foldchar, 8)
  let foldtextlength = strlen(substitute(foldtextstart . foldtextend, '.', 'x', 'g')) + &foldcolumn
  return foldtextstart . repeat(foldchar, winwidth(0)-foldtextlength) . foldtextend
endfun
set foldtext=NeatFoldText()
