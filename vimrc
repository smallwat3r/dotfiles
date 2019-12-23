" File              : vimrc
" Author            : smallwat3r
" Date              : Wed  7 Aug 19:49:35 2019
"
" Vim config.
"

" ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" Plugins
" ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

" Using vim-plug
call plug#begin()

Plug 'dense-analysis/ale'
Plug 'jiangmiao/auto-pairs'
Plug 'smallwat3r/vim-efficient'
Plug 'smallwat3r/vim-simplicity'
Plug 'Chiel92/vim-autoformat'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
Plug 'alpertuna/vim-header'
Plug 'Vimjas/vim-python-pep8-indent'
Plug 'tpope/vim-vinegar'
Plug 'dhruvasagar/vim-table-mode'
Plug 'tpope/vim-surround'
Plug 'alvan/vim-closetag'
Plug 'gregsexton/MatchTag'
Plug 'tweekmonster/impsort.vim'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

call plug#end()

" ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" Plugins configs
" ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

" vim git-gutter
let g:gitgutter_sign_added='+'
let g:gitgutter_sign_modified='~'
let g:gitgutter_sign_removed='-'

" Auto Headers on F5
let g:header_auto_add_header=0
let g:header_field_author='Matthieu Petiteau'
let g:header_field_author_email='mpetiteau.pro@gmail.com'
let g:header_field_timestamp_format='%d.%m.%Y'
let g:header_field_modified_timestamp=0
let g:header_field_modified_by=0
map <F5> :AddHeader<CR>

" Ale
let g:ale_echo_msg_error_str='E'
let g:ale_echo_msg_warning_str='W'
let g:ale_echo_msg_format='[%linter%] %s [%severity%]'
let g:ale_pattern_options={
      \ '\.min\.js$': {'ale_linters': [], 'ale_fixers': []},
      \ '\.min\.css$': {'ale_linters': [], 'ale_fixers': []}, }

" Impsort - Python imports sorting on save.
autocmd BufWritePre *.py ImpSort!

" ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" General configs
" ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

syntax on
filetype plugin indent on

" Indents (4 spaces by default)
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
set autoindent
set smartindent
autocmd FileType make set tabstop=8 shiftwidth=8 softtabstop=0 noexpandtab

" Remap leader
let mapleader=','

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
set listchars=tab:➝\
set listchars+=eol:¬
set listchars+=extends:>
set listchars+=precedes:<

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

" Colors
set t_Co=256
set bg=dark
colo simplicity

" Gui settings
if (has("gui_running"))
  set linespace=0
  set guifont=MonacoB2:h13
  " set transparency=5
  set guioptions-=mTrL  " remove all GUI widgets
  " set gcr=a:blinkon0    " no blinking cursor
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

" Remove trailing whitespaces on save, except certain filetype
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
        \   all_errors )
endfun

" Folds format
fun! CustomFoldText()
  let line = ' ' . substitute(getline(v:foldstart), '^\s*"\?\s*\|\s*"\?\s*{{' . '{\d*\s*', '', 'g') . ' '
  let lines_count = v:foldend - v:foldstart + 1
  let lines_count_text = printf("%10s", '(' . lines_count . ')') . ' .'
  let foldchar = matchstr(&fillchars, 'fold:\zs.')
  let foldtextend = strpart(repeat(foldchar, v:foldlevel*2) . line, 0, (winwidth(0)*2)/3)
  let foldtextstart = '+ ' . lines_count_text . repeat(foldchar, 8)
  let foldtextlength = strlen(substitute(foldtextstart . foldtextend, '.', 'x', 'g')) + &foldcolumn
  return foldtextstart . repeat(foldchar, winwidth(0)-foldtextlength) . foldtextend
endfun
set foldtext=CustomFoldText()
