" Author: Matthieu Petiteau <mpetiteau.pro@gmail.com>
" neovim config file

"{{{ plugin manager (vim-plug)

" Auto load for first time use - Install Vim Plug Manager
if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
        \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin()

Plug 'dense-analysis/ale'      " Ale code linter
Plug 'chr4/nginx.vim'          " Nginx / Jinja syntax
Plug 'sbdchd/neoformat'        " Auto code formatting
Plug 'tpope/vim-commentary'    " Comments mappings
Plug 'tpope/vim-eunuch'        " Shell commands from vim
Plug 'tpope/vim-fugitive'      " Git wrapper
Plug 'junegunn/gv.vim'         " Commits browser
Plug 'mhinz/vim-signify'       " Git signs
Plug 'tpope/vim-vinegar'       " File browser
Plug 'alvan/vim-closetag'      " Auto-close html tags
Plug 'gregsexton/MatchTag'     " Highlight matching html tag
Plug 'machakann/vim-sandwich'  " Surroundings mapping
Plug 'tpope/vim-unimpaired'    " Complementary mappings
Plug 'chrisbra/csv.vim'        " CSV files
Plug 'simnalamburt/vim-mundo'  " Undo tree
Plug 'zirrostig/vim-schlepp'   " Move visual blocks

" Fuzzy finder
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

" Text completion
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'Shougo/context_filetype.vim'
Plug 'Shougo/neopairs.vim'
Plug 'Shougo/neoinclude.vim'
Plug 'deoplete-plugins/deoplete-dictionary'

call plug#end()

"}}} plugin manager (vim-plug)
"{{{ plugins configuration

" Signify
let g:signify_sign_add='ᵃ'
let g:signify_sign_delete='ᵈ'
let g:signify_sign_change='ᵐ'

" Deoplete
let g:deoplete#enable_at_startup=1

" Ale
let g:ale_echo_msg_error_str='E'
let g:ale_echo_msg_warning_str='W'
let g:ale_set_highlights=0
let g:ale_sign_error='ˣˣ'
let g:ale_sign_warning='ʷʷ'
let g:ale_echo_msg_format='[%linter%] %s [%severity%]'

function! LinterStatus() abort
  " Show Ale linter erros in statusline
  let l:counts=ale#statusline#Count(bufnr(''))
  let l:all_errors=l:counts.error + l:counts.style_error
  let l:all_non_errors=l:counts.total - l:all_errors
  return l:counts.total == 0 ? 'OK' : printf(
        \   '%dW %dE',
        \   all_non_errors,
        \   all_errors )
endfunction

" Neoformat
let g:neoformat_basic_format_align=1
let g:neoformat_basic_format_retab=1
let g:neoformat_basic_format_trim=1

let g:neoformat_python_black = {
      \ 'exe': 'black',
      \ 'stdin': 1,
      \ 'args': ['-q', '-', '-l 79'],
      \ }
let g:neoformat_javascript_prettier = {
      \ 'exe': 'prettier',
      \ 'stdin': 1,
      \ 'args': ['--stdin', '--print-width 110', '--stdin-filepath', '"%:p"'],
      \ }
let g:neoformat_html_prettier = {
      \ 'exe': 'prettier',
      \ 'stdin': 1,
      \ 'args': ['--stdin', '--print-width 110', '--stdin-filepath', '"%:p"'],
      \ }
let g:neoformat_htmldjango_prettier = {
      \ 'exe': 'prettier',
      \ 'stdin': 1,
      \ 'args': ['--stdin', '--print-width 110', '--stdin-filepath', '"%:p"'],
      \ }

let g:neoformat_enabled_python = ['black']
let g:neoformat_enabled_javascript = ['prettier']
let g:neoformat_enabled_html = ['prettier']
let g:neoformat_enabled_htmldjango = ['prettier']
let g:neoformat_enabled_zsh = ['shfmt']
let g:shfmt_opt='-ci'  " shell

" FZF
command! -bang -nargs=? -complete=dir Files
      \ call fzf#vim#files(
      \ <q-args>, {'options': ['--layout=reverse', '--info=inline', '--preview',
      \ '~/.config/nvim/plugged/fzf.vim/bin/preview.sh {}']}, <bang>0)

command! -bang -nargs=* Rg
      \ call fzf#vim#grep(
      \   'rg --column --line-number --no-heading --color=always --smart-case '.shellescape(<q-args>), 1,
      \   fzf#vim#with_preview(), <bang>0)

" vim sandwich
let g:sandwich#recipes = deepcopy(g:sandwich#default_recipes)
let g:sandwich#recipes += [
      \   {
      \     'buns'        : ['{', '}'],
      \     'motionwise'  : ['line'],
      \     'kind'        : ['add'],
      \     'linewise'    : 1,
      \     'command'     : ["'[+1,']-1normal! >>"],
      \   },
      \   {
      \     'buns'        : ['{', '}'],
      \     'motionwise'  : ['line'],
      \     'kind'        : ['delete'],
      \     'linewise'    : 1,
      \     'command'     : ["'[,']normal! <<"],
      \   }
      \ ]

"}}} plugins configuration
"{{{ general configuration

syntax on
filetype plugin indent on

let mapleader=','  " Leader key

set encoding=utf-8
set fileencoding=utf-8
set updatetime=100      " async update time
set hidden              " hide buffer instead of closing them
set cmdheight=1         " height of cmd line
set nomodeline          " ignore modelines for security reasons
set autoread            " reread changed files automatically
set laststatus=2        " always show statusline
set noshowcmd           " don't show command in last line of screen
set noruler             " don't show cursor position
set nu rnu              " relative line numbers and current line number
set showmatch           " matching brackets
set mouse=a             " mouse support
set incsearch           " search pattern
set hlsearch            " search highlighting
set wrap                " wrap lines
set lazyredraw          " no redraw
set ignorecase          " search ignore case
set scrolljump=8        " minimal nb of lines to scroll when cursor gets off the screen
set nocompatible        " modern vim
set wildignorecase      " ignore case when completing filenames and directories
set noshowmode          " do not show vim mode (already in statusline)
set splitbelow          " for ex preview windows will appear at the bottom
set noshowmode          " don't show mode (already in statusline)
set inccommand=nosplit  " show replacements using search / replace
set nocursorcolumn      " don't highlight column
set nocursorline        " don't highlight line
set shortmess+=c        " silence message completion menu
set nolist              " hide special characters
set diffopt+=vertical   " diff splits
set visualbell t_vb=    " deactivate bells and alerts
set showbreak=⤿\        " line break symbol
set foldmethod=marker   " use marker to fold lines
set synmaxcol=200       " keep longlines from slowing vim
set fillchars=vert:┃
set listchars=tab:→\ ,eol:¬,extends:>,precedes:<,nbsp:˷,trail:␣
set matchpairs+=<:>,«:»,｢:｣
set clipboard+=unnamedplus
set spellfile=~/.config/nvim/spell/en.utf-8.add

" Ignore files and folders
set wildignore=*.swp,*.bak,*.pyc,*.class,*.cache,*.dll,*.DS_Store,*.rdb,*.db,*.sqlite
set wildignore+=__pycache__/*,venv/*,env/*,.git/*,build/*,node_modules/*,dist/*

" No swap files / backups etc
set noswapfile
set nobackup
set directory=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp

" Undo
set undolevels=4000
set undoreload=100000
set undodir=~/.config/nvim/undodir
set undofile

" Backspace as it should work
set backspace=indent,eol,start
set whichwrap+=<,>,h,l

" Netrw settings
let g:netrw_banner=0
let g:netrw_sort_sequence='[\/]$,*'
let g:netrw_localrmdir='rm -r'

" Deactivate db text plugin error msg on sql completion
let g:loaded_sql_completion=0
let g:omni_sql_no_default_maps=1

" Save as root
command! WW :w !sudo tee % >/dev/null

" Source on save config
au BufWritePost ~/.config/nvim/init.vim source ~/.config/nvim/init.vim

" close method preview window after completion is complete
au InsertLeave,CompleteDone * if pumvisible() == 0 | pclose | endif

" load custom dict files (with deoplete-dictionary)
au FileType * execute 'setlocal dict+=~/.config/nvim/dict/' . &filetype . '.txt'
call deoplete#custom#source('dictionary', 'matchers', ['matcher_head'])
call deoplete#custom#source('dictionary', 'sorters', [])
call deoplete#custom#source('dictionary', 'min_pattern_length', 2)

" Remove trailing whitespaces
function! TrimTrailingWS()
  if exists('b:noStripWhitespace')
    return
  endif
  if search('\s\+$', 'cnw')
    :%s/\s\+$//g
  endif
endfunction
au BufWritePre * :call TrimTrailingWS()
au FileType markdown let b:noStripWhitespace=1

" Italics
let &t_ZH='\e[3m'
let &t_ZR='\e[23m'

" Change cursor based on modes
let &t_SI.='\e[6 q' " INSERT mode
let &t_SR.='\e[4 q' " REPLACE mode
let &t_EI.='\e[2 q' " NORMAL mode or others

" Fix weird Python indent behaviour
let g:pyindent_open_paren='0'
let g:pyindent_nested_paren='&sw'
let g:pyindent_continue='&sw'

" Special filetypes
au BufRead,BufNewFile *.md set ft=markdown
au BufRead,BufNewFile */nginx/*.conf    set ft=nginx
au BufRead,BufNewFile */nginx/**/*.conf set ft=nginx
au BufRead,BufNewFile *.{yaml,yml} set ft=yaml
au BufRead,BufNewFile gitconfig set ft=gitconfig

" Special options by filetypes
au FileType gitcommit setlocal spell
au FileType markdown setlocal spell list
au FileType text setlocal spell
au FileType netrw setlocal bufhidden=delete

" Indentation
set expandtab
set shiftwidth=2
set tabstop=2
set softtabstop=2
au FileType make setlocal ts=8 sw=8 noet
au FileType go setlocal ts=8 sw=8 noet
au FileType python setlocal ts=4 sw=4 sts=4 et
au FileType perl setlocal ts=4 sw=4 sts=4 et

" Remember last location
au BufReadPost *
      \ if line("'\"") > 0 && line("'\"") <= line("$") |
      \   exe "normal g'\"" |
      \ endif

"}}} general configuration
"{{{ mappings / keybindings

" Normal mode mappings
" **********************

" navigate to end and start of line
nmap B ^
nmap E $

" navigate between brackets
nmap <tab> %

" align paragraph
nmap <leader>a =ip
" copy paragraph
nmap cp yap<S-}>p

" delete to blackhole register (don't lose previous yank)
nmap s "_d
nmap ss "_dd

" swap with next or previous character
nmap ]c xph
nmap [c hxp

" remove search highlight
map <silent><leader><space> :nohlsearch<cr>

" edit config file
nmap <leader>e :e! $MYVIMRC<cr>

" source current file
nmap <silent><leader>so :so %<cr>:echo 'File sourced'<cr>

" cd into current buffer directory
nmap <silent><leader>cd :cd %:p:h<cr>:pwd<cr>
" cd into previous directory
nmap <silent><leader>cdp :cd ..<cr>:pwd<cr>
" pwd
nmap <leader>d :pwd<cr>

" delete current buffer
nmap <silent>;d :bp\|bd #<cr>:echo 'Buffer deleted'<cr>

" toggle spell
nmap <leader>sp :setlocal spell!<cr>

" quick write & quit
nmap ;w :w<cr>
nmap ;q :q!<cr>
nmap ;x :x<cr>

" splits
nmap ;sp :sp<cr>
nmap ;vs :vs<cr>

" quick substitutes (whole file)
nmap ;s/ :%s///g<left><left><left>

" format file
nmap ;f :Neoformat<cr>

" FZF
nmap <leader>b :Buffers<cr>
nmap <leader>f :Files<cr>
nmap <leader>l :BLines<cr>
nmap <leader>; :Rg<cr>
nmap <leader>co :Commits<cr>

" navigate window panels
nmap <C-h> <C-w>h
nmap <C-j> <C-w>j
nmap <C-k> <C-w>k
nmap <C-l> <C-w>l

" extend previous search
nmap // /<C-R>/

" window scroll
nmap <A-j> <C-e>
nmap <A-k> <C-y>

" case insensitive replace word (aka multiple cursors)
nmap <leader>x /\<<C-R>=expand('<cword>')<cr>\>\C<cr>``cgn
nmap <leader>X ?\<<C-R>=expand('<cword>')<cr>\>\C<cr>``cgN

" resize splits
nmap <silent><S-k> :res +5<cr>
nmap <silent><S-j> :res -5<cr>
nmap <silent><S-h> :vertical resize-5<cr>
nmap <silent><S-l> :vertical resize+5<cr>

" play macros
nmap Q @q

" toggle undo tree
nmap <leader>u :MundoToggle<CR>

" Visual mode mappings
" **********************

" play macros with visual mode
vmap Q :norm @q<cr>

" crtl + hjkl move visual blocks
vmap <C-h> <Plug>SchleppLeft
vmap <C-j> <Plug>SchleppDown
vmap <C-k> <Plug>SchleppUp
vmap <C-l> <Plug>SchleppRight

" keep visual selection when re-indenting
xmap > >gv
xmap < <gv

" vaa select the entire file
xmap aa VGo1G

" Insert mode mappings
" **********************

" tab completion
imap <expr> <tab>   pumvisible() ? '<c-n>' : '<tab>'
imap <expr> <s-tab> pumvisible() ? '<c-p>' : '<s-tab>'

" jj works as ESC
imap jj <esc>

" auto close matching pairs
imap { {<esc>i<right>}<esc>i
imap ( (<esc>i<right>)<esc>i
imap [ [<esc>i<right>]<esc>i

" auto close matching pairs multi line
imap {<cr> {<cr>}<esc>ko<tab>
imap [<cr> [<cr>]<esc>ko<tab>
imap (<cr> (<cr>)<esc>ko<tab>

" crtl + hjkl cursor movement on insert mode
imap <C-h> <left>
imap <C-j> <down>
imap <C-k> <up>
imap <C-l> <right>

" Command mode mappings
" **********************

" crtl + hjkl cursor movement on command mode
cmap <C-h> <left>
cmap <C-j> <down>
cmap <C-k> <up>
cmap <C-l> <right>

" Useful insert mode abbr completion
" **********************

" Personal stuff
iab em,, mpetiteau.pro@gmail.com
iab ma,, Matthieu Petiteau
iab au,, Matthieu Petiteau <mpetiteau.pro@gmail.com>
iab sign,, --<cr>Matthieu Petiteau<cr>mpetiteau.pro@gmail.com
iab cc,, Copyright 2020 Matthieu Petiteau, all rights reserved.

" Shebang
iab sba,, #!/usr/bin/env bash
iab spe,, #!/usr/bin/env perl
iab spy,, #!/usr/bin/env python3

" Current date / datetime / timestamp ISO8601/W3C
iab dd,, <C-R>=strftime("%a, %d %b %Y")<cr>
iab dt,, <C-R>=strftime("%a, %d %b %Y at %H:%M")<cr>
iab ts,, <C-R>=strftime("%FT%T%z")<cr>

" Line separators
iab --,, -----------------------------------------------------------------------------
iab ##,, #############################################################################
iab //,, /////////////////////////////////////////////////////////////////////////////
iab **,, *****************************************************************************
iab @@,, @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
iab ++,, +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

"}}} mappings / keybindings
"{{{ colors

" Colorscheme
colo desert

" Overwrite colors on general stuff
hi Normal            ctermfg=15
hi Number            ctermfg=15
hi String            ctermfg=194
hi Folded            ctermfg=231    ctermbg=239
hi MatchParen        ctermfg=237    ctermbg=200
hi SignColumn        ctermfg=NONE   ctermbg=NONE cterm=NONE
hi LineNr            ctermfg=239    ctermbg=NONE
hi VertSplit         ctermfg=240    ctermbg=NONE cterm=NONE
hi StatuslineNC      ctermfg=250    ctermbg=238  cterm=NONE
hi Search            ctermfg=232    ctermbg=226
hi IncSearch         ctermfg=232    ctermbg=231

" Diff colors
hi DiffAdd           ctermfg=255    ctermbg=64
hi DiffChange        ctermfg=NONE   ctermbg=NONE cterm=NONE
hi DiffDelete        ctermfg=167    ctermbg=NONE cterm=NONE
hi DiffText          ctermfg=255    ctermbg=24

" Signify colors
hi SignifySignAdd    ctermfg=green  cterm=NONE
hi SignifySignDelete ctermfg=red    cterm=NONE
hi SignifySignChange ctermfg=yellow cterm=NONE

" Ale colors
hi ALEErrorSign      ctermfg=red    ctermbg=NONE
hi ALEWarningSign    ctermfg=yellow ctermbg=NONE

"}}} colors
"{{{ statusline

" Show git info in statusline (with fugitive)
function! GitInfo()
  if fugitive#head() != ''
    return ' (on '.fugitive#head().') '
  endif
  return ''
endfunction

" Statusline colors depending on mode
hi NormalColor  ctermbg=15  ctermfg=0
hi InsertColor  ctermbg=85  ctermfg=0
hi ReplaceColor ctermbg=180 ctermfg=0
hi VisualColor  ctermbg=208 ctermfg=0
hi CommandColor ctermbg=204 ctermfg=0

" Manage statusline colors from vim mode
function! ColorMode()
  if (mode() =~# '\v(n|no)')
    return '%#NormalColor# NOR'
  elseif mode() == 'i'
    return '%#InsertColor# INS'
  elseif mode() == 'R'
    return '%#ReplaceColor# REP'
  elseif (mode() =~# '\v(v|V)')
    return '%#VisualColor# VIS'
  elseif mode() == 'c'
    return '%#CommandColor# CMD'
  endif
  return ''
endfunction

" Statusline format
function! StatusLineFmt(active)
  let sl = ''
  if a:active
    let sl.=ColorMode()
    let sl.='%{GitInfo()} %{LinterStatus()}'
  endif
  let sl.=' %n %t%{&modified?"\ (+)":""}%{&readonly?"\ (ro)":""}'
  let sl.=' %=%-14.(%l,%c%) %{&filetype} %{strlen(&fenc)?&fenc:&enc} '
  return sl
endfunction

" Active and non-active on window change event
augroup status
  au WinEnter * setlocal statusline=%!StatusLineFmt(1)
  au WinLeave * setlocal statusline=%!StatusLineFmt(0)
augroup END

" Set statusline (1 = active by default)
set statusline=%!StatusLineFmt(1)

"}}} statusline
