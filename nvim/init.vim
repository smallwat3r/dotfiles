" neovim config file
" Matthieu Petiteau <mpetiteau.pro@gmail.com>

"{{{1 plugins

"{{{2 vim-plug

" Auto load for first time use - Install Vim Plug Manager
if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
        \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  au VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin()

Plug 'sbdchd/neoformat'                " Auto code formatting
Plug 'tpope/vim-commentary'            " Comments mappings
Plug 'tpope/vim-eunuch'                " Shell commands from vim
Plug 'tpope/vim-fugitive'              " Git wrapper
Plug 'junegunn/gv.vim'                 " Commits browser
Plug 'alvan/vim-closetag'              " Auto-close html tags
Plug 'gregsexton/MatchTag'             " Highlight matching html tag
Plug 'machakann/vim-sandwich'          " Surroundings mapping
Plug 'tpope/vim-unimpaired'            " Complementary mappings
Plug 'simnalamburt/vim-mundo'          " Undo tree
Plug 'zirrostig/vim-schlepp'           " Move visual blocks
Plug 'cocopon/vaffle.vim'              " File browsing
Plug 'christoomey/vim-tmux-navigator'  " Tmux navigation

" Syntax support
Plug 'chrisbra/csv.vim'
Plug 'cespare/vim-toml'
Plug 'chr4/nginx.vim'

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

"}}}2 vim-plug
"{{{2 plugins configuration

"{{{3 deoplete

let g:deoplete#enable_at_startup=1

" Dictionaries
call deoplete#custom#source('dictionary', 'matchers', ['matcher_head'])
call deoplete#custom#source('dictionary', 'sorters', [])
call deoplete#custom#source('dictionary', 'min_pattern_length', 2)

"}}}3 deoplete
"{{{3 neoformat

let g:neoformat_basic_format_align=1
let g:neoformat_basic_format_retab=1
let g:neoformat_basic_format_trim=1

let g:neoformat_python_black = {
      \ 'exe': 'black',
      \ 'stdin': 1,
      \ 'args': ['-q', '-', '-l 89'],
      \ }
let g:neoformat_python_yapf = {
      \ 'exe': 'yapf',
      \ 'stdin': 1,
      \ 'args': ['--style', 'pep8'],
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

let g:neoformat_enabled_python = ['yapf', 'black']
let g:neoformat_enabled_javascript = ['prettier']
let g:neoformat_enabled_html = ['prettier']
let g:neoformat_enabled_htmldjango = ['prettier']
let g:neoformat_enabled_zsh = ['shfmt']

let g:shfmt_opt='-ci'  " shell

"}}}3 neoformat
"{{{3 fzf

com! -bang -nargs=? -complete=dir Files
      \ call fzf#vim#files(
      \ <q-args>, {'options': ['--layout=reverse', '--info=inline', '--preview',
      \ '~/.config/nvim/plugged/fzf.vim/bin/preview.sh {}']}, <bang>0)

com! -bang -nargs=* Rg
      \ call fzf#vim#grep(
      \   'rg --column --line-number --no-heading --color=always --smart-case '.shellescape(<q-args>), 1,
      \   fzf#vim#with_preview(), <bang>0)

"}}}3 fzf
"{{{3 vim sandwich

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

"}}}3 vim sandwich
"{{{3 vaffle

" mappings for vaffle to work as netrw / vinegar
function! s:customize_vaffle_mappings() abort
  nmap <buffer>- <Plug>(vaffle-open-parent)
  nmap <buffer>% <Plug>(vaffle-new-file)
  nmap <buffer>d <Plug>(vaffle-mkdir)
  nmap <buffer>D <Plug>(vaffle-delete-selected)
endfunction

augroup vaffle_mappings
  au!
  au FileType vaffle call s:customize_vaffle_mappings()
augroup END

let g:vaffle_show_hidden_files=1
let g:vaffle_force_delete=1

"}}}3 vaffle

"}}}2 plugins configuration

"}}}1 plugins
"{{{1 general configuration

syntax on
filetype plugin indent on
let mapleader=','  " Leader key

"{{{2 encoding

set encoding=utf-8
set fileencoding=utf-8

"}}}2 encoding
"{{{2 miscellaneous

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
set synmaxcol=200       " keep long lines from slowing vim
set fillchars=vert:┃
set listchars=tab:→\ ,eol:¬,extends:>,precedes:<,nbsp:˷,trail:␣
set matchpairs+=<:>
set clipboard+=unnamedplus
set spellfile=~/.config/nvim/spell/en.utf-8.add
set backspace=indent,eol,start
set whichwrap+=<,>,h,l

"}}}2 miscellaneous
"{{{2 ignore files

set wildignore=*.swp,*.bak,*.pyc,*.class,*.cache,*.dll,*.DS_Store,*.rdb,*.db,*.sqlite
set wildignore+=__pycache__/*,venv/*,env/*,.git/*,build/*,node_modules/*,dist/*

"}}}2 ignore files
"{{{2 swapfiles

set noswapfile
set nobackup
set directory=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp

"}}}2 swapfiles
"{{{2 undo

set undolevels=4000
set undoreload=100000
set undodir=~/.config/nvim/undodir
set undofile

"}}}2 undo
"{{{2 trim whitespaces

" Remove trailing whitespaces
function! TrimTrailingWS()
  if exists('b:noStripWhitespace')
    return
  endif
  if search('\s\+$', 'cnw')
    :%s/\s\+$//g
  endif
endfunction

augroup trim_whitespaces
  au!
  au BufWritePre * :call TrimTrailingWS()
augroup END

"}}}2 trim whitespaces
"{{{2 indentation

set expandtab
set shiftwidth=2
set tabstop=2
set softtabstop=2

"}}}2 indentation
"{{{2 filetypes specifics

augroup filetype_detect
  au!
  au BufRead,BufNewFile *.md setf markdown
  au BufRead,BufNewFile */nginx/*.conf setf nginx
  au BufRead,BufNewFile */nginx/**/*.conf setf nginx
  au BufRead,BufNewFile *.{yaml,yml} setf yaml
  au BufRead,BufNewFile gitconfig setf gitconfig
  au BufRead,BufNewFile *.sketch setf sketch
  au BufRead,BufNewFile Dockerfile.* setf dockerfile
  au BufRead,BufNewFile *.{dockerfile,Dockerfile} setf dockerfile
augroup END

augroup filetype_indentation
  au!
  au FileType make setl ts=8 sw=8 noet
  au FileType go setl ts=8 sw=8 noet
  au FileType python setl ts=4 sw=4 sts=4 et
  au FileType perl setl ts=4 sw=4 sts=4 et
augroup END

augroup filetype_specifics
  au!
  au FileType gitcommit setl spell
  au FileType markdown setl spell list | let b:noStripWhitespace=1
  au FileType sketch setl spell
  au FileType netrw setl bufhidden=delete
augroup END

augroup filetype_dictionaries
  au!
  au Filetype python,sql,javascript,html,go,dockerfile,css
        \ execute 'setl dict+=~/.config/nvim/dict/' . &filetype . '.txt'
augroup END

"}}}2 filetypes specifics
"{{{2 netrw

let g:netrw_banner=0
let g:netrw_sort_sequence='[\/]$,*'
let g:netrw_localrmdir='rm -r'

"}}}2 netrw
"{{{2 fixes

" Deactivate db text plugin error msg on sql completion
let g:loaded_sql_completion=0
let g:omni_sql_no_default_maps=1

" Fix weird Python indent behaviour
let g:pyindent_open_paren='0'
let g:pyindent_nested_paren='&sw'
let g:pyindent_continue='&sw'

"}}}2 fixes
"{{{2 other

" Remember last position of the cursor when editing a file
augroup remember_last_cursor_position
  au!
  au BufReadPost *
        \ if line("'\"") > 0 && line ("'\"") <= line("$") |
        \   exe "normal! g'\"" |
        \ endif
augroup END

" Disable auto insertion of comment symbol on new line
augroup disable_comment_auto_insert
  au!
  au BufNewFile,BufRead * setlocal formatoptions-=cro
augroup END

"}}}2 other

"}}}1 general configuration
"{{{1 mappings / keybindings

"{{{2 normal mode

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
nmap <leader>sp :setl spell!<cr>

" quick write & quit
nmap ;w :w<cr>
nmap ;q :q!<cr>
nmap ;x :x<cr>

" splits
nmap ;sp :sp<cr>
nmap ;vs :vs<cr>

" quick substitutes (whole file)
nmap ;s/ :%s///g<left><left><left>

" file browsing
nmap <silent>- :Vaffle<cr>

" format file
nmap ;f :Neoformat<cr>

" git logs current file
nmap <leader>gl :GV!<cr>

" fzf
nmap <leader>b :Buffers<cr>
nmap <leader>f :Files<cr>
nmap <leader>; :Rg<cr>
nmap <leader>w; :exe ":Rg " . expand('<cword>')<cr>
nmap <leader>co :Commits<cr>
nmap <leader>l :BLines<cr>
nmap <leader>wl :exe ":BLines " . expand('<cword>')<cr>

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

"}}}2 normal mode
"{{{2 visual mode

" remap easier access to visual blocks
nnoremap <C-v> v
nnoremap v <C-v>

" play macros with visual mode
vmap Q :norm @q<cr>

" crtl + hjkl move visual blocks
vmap <C-h> <Plug>SchleppLeft
vmap <C-j> <Plug>SchleppDown
vmap <C-k> <Plug>SchleppUp
vmap <C-l> <Plug>SchleppRight

" wrap visually selected
xmap (( <esc>`>a)<esc>`<i(<esc>
xmap {{ <esc>`>a}<esc>`<i{<esc>
xmap "" <esc>`>a"<esc>`<i"<esc>
xmap '' <esc>`>a'<esc>`<i'<esc>
xmap `` <esc>`>a`<esc>`<i`<esc>

" keep visual selection when re-indenting
xmap > >gv
xmap < <gv

" vaa select the entire file
xmap aa VGo1G

"}}}2 visual mode
"{{{2 insert mode

" tab completion
imap <expr> <tab>   pumvisible() ? '<c-n>' : '<tab>'
imap <expr> <s-tab> pumvisible() ? '<c-p>' : '<s-tab>'

" jj works as esc
imap jj <esc>

" hjkl insert go-to (new line or end/start of line)
imap 1h <esc>I
imap 1j <esc>o
imap 1k <esc>O
imap 1l <esc>A

" auto close matching pairs
imap '<tab> ''<left>
imap `<tab> ``<left>
imap "<tab> ""<left>
imap (<tab> ()<left>
imap [<tab> []<left>
imap {<tab> {}<left>

" auto close matching pairs on multi lines
imap {<cr> {<cr>}<esc>ko<tab>
imap [<cr> [<cr>]<esc>ko<tab>
imap (<cr> (<cr>)<esc>ko<tab>

" crtl + hjkl cursor movement on insert mode
imap <C-h> <left>
imap <C-j> <down>
imap <C-k> <up>
imap <C-l> <right>

" Delete a word
imap <C-d> <ESC>ciw

"}}}2 insert mode
"{{{2 command mode

" crtl + hjkl cursor movement on command mode
cmap <C-h> <left>
cmap <C-j> <down>
cmap <C-k> <up>
cmap <C-l> <right>

"}}}2 command mode
"{{{2 insert mode abbreviations

" Personal stuff
iab @e, mpetiteau.pro@gmail.com
iab @a, Matthieu Petiteau <mpetiteau.pro@gmail.com>
iab @c, Copyright 2020 Matthieu Petiteau, all rights reserved.

" Shebang
iab @s, #!/usr/bin/env
iab @sh, #!/usr/bin/env bash
iab @pe, #!/usr/bin/env perl
iab @py, #!/usr/bin/env python3

" Current date / datetime / timestamp ISO8601/W3C
iab @d, <C-R>=strftime("%a, %d %b %Y")<cr>
iab @dt, <C-R>=strftime("%a, %d %b %Y at %H:%M")<cr>
iab @ts, <C-R>=strftime("%FT%T%z")<cr>

" Line separators
iab @-, -----------------------------------------------------------------
iab @#, #################################################################
iab @*, *****************************************************************

"}}}2 insert mode abbreviations

"}}}1 mappings / keybindings
"{{{1 theme

"{{{2 terminal

" Italics
let &t_ZH='\e[3m'
let &t_ZR='\e[23m'

" Change cursor based on modes
let &t_SI.='\e[6 q' " INSERT mode
let &t_SR.='\e[4 q' " REPLACE mode
let &t_EI.='\e[2 q' " NORMAL mode or others

"}}}2 terminal
"{{{2 colors

function! CustomColors() abort
  if &diff
    syntax off
  endif

  hi Normal        ctermfg=15
  hi Number        ctermfg=15
  hi String        ctermfg=180
  hi Folded        ctermfg=231  ctermbg=239
  hi MatchParen    ctermfg=231  ctermbg=199
  hi SignColumn    ctermfg=NONE ctermbg=NONE cterm=NONE
  hi LineNr        ctermfg=239  ctermbg=NONE
  hi VertSplit     ctermfg=240  ctermbg=NONE cterm=NONE
  hi StatuslineNC  ctermfg=250  ctermbg=238  cterm=NONE
  hi Search        ctermfg=232  ctermbg=226
  hi IncSearch     ctermfg=232  ctermbg=231

  hi DiffAdd       ctermfg=255  ctermbg=64
  hi DiffChange    ctermfg=204  ctermbg=NONE cterm=NONE
  hi DiffDelete    ctermfg=red  ctermbg=NONE cterm=NONE
  hi DiffText      ctermfg=255  ctermbg=31

  " Custom statusline colors
  hi SLNormalColor   ctermbg=15  ctermfg=0
  hi SLInsertColor   ctermbg=85  ctermfg=0
  hi SLReplaceColor  ctermbg=180 ctermfg=0
  hi SLVisualColor   ctermbg=208 ctermfg=0
  hi SLCommandColor  ctermbg=204 ctermfg=0
  hi SLTerminalColor ctermbg=228 ctermfg=0
endfunction

augroup custom_colors
  au!
  au ColorScheme * call CustomColors()
augroup END

colo desert

"}}}2 colors

"}}}1 theme
"{{{1 statusline

" Show git info in statusline (with fugitive)
function! GitInfo()
  if fugitive#head() != ''
    return ' (on ' . fugitive#head() . ')'
  endif
  return ''
endfunction

" Manage statusline colors from vim mode
function! ColorMode()
  if (mode() =~# '\v(n|no)')
    return '%#SLNormalColor# %n NOR'
  elseif (mode() ==# 'i')
    return '%#SLInsertColor# %n INS'
  elseif (mode() ==# 'R')
    return '%#SLReplaceColor# %n REP'
  elseif (mode() =~# '\v(v|V)') || (mode() == "\<C-v>")
    return '%#SLVisualColor# %n VIS'
  elseif (mode() ==# 'c')
    return '%#SlCommandColor# %n CMD'
  elseif (mode() ==# 't')
    return '%#SlTerminalColor# %n TER'
  endif
  return ''
endfunction

" Statusline format
function! StatusLineFmt(active)
  let sl = ''
  if a:active
    let sl.=ColorMode()
    let sl.='%{GitInfo()}'
  endif
  let sl.=' %t%{&modified?"\ (+)":""}%{&readonly?"\ (ro)":""}'
  let sl.=' %=%-14.(%l,%c%) %{&filetype} %{strlen(&fenc)?&fenc:&enc} '
  return sl
endfunction

" Active and non-active on window change event
augroup status
  au!
  au WinEnter * setl statusline=%!StatusLineFmt(1)
  au WinLeave * setl statusline=%!StatusLineFmt(0)
augroup END

" Set statusline (1 = active by default)
set statusline=%!StatusLineFmt(1)

"}}}1 statusline
