" File  : init.vim
" Author: Matthieu Petiteau <mpetiteau.pro@gmail.com>
"
" neovim config file

" #####################################################
" PLUGINS MANAGER (vim-plug)                          #
" #####################################################

" Auto load for first time use - Install Vim Plug Manager
" --------------------------------------------------------------------
if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
        \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin()

Plug 'dense-analysis/ale'
Plug 'sbdchd/neoformat'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'junegunn/gv.vim'
Plug 'mhinz/vim-signify'
Plug 'tpope/vim-vinegar'
Plug 'dhruvasagar/vim-table-mode'
Plug 'alvan/vim-closetag'
Plug 'gregsexton/MatchTag'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'Vimjas/vim-python-pep8-indent'
Plug 'machakann/vim-sandwich'
Plug 'tpope/vim-unimpaired'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'Shougo/context_filetype.vim'
Plug 'Shougo/neopairs.vim'
Plug 'Shougo/neoinclude.vim'
Plug 'deoplete-plugins/deoplete-dictionary'

call plug#end()

" #####################################################
" PLUGINS CONFIG                                      #
" #####################################################

" signify
let g:signify_sign_add='›'
let g:signify_sign_delete='›'
let g:signify_sign_change='›'

" use deoplete
let g:deoplete#enable_at_startup=1

" Ale
let g:ale_echo_msg_error_str='E'
let g:ale_echo_msg_warning_str='W'
let g:ale_set_highlights=0
let g:ale_sign_error='!'
let g:ale_sign_warning='?'
let g:ale_echo_msg_format='[%linter%] %s [%severity%]'
function! LinterStatus() abort
  " Show linter erros in statusline
  let l:counts=ale#statusline#Count(bufnr(''))
  let l:all_errors=l:counts.error + l:counts.style_error
  let l:all_non_errors=l:counts.total - l:all_errors
  return l:counts.total == 0 ? 'OK' : printf(
        \   '%dW %dE',
        \   all_non_errors,
        \   all_errors )
endfunction

" neoformat
let g:neoformat_basic_format_align=1
let g:neoformat_basic_format_retab=1
let g:neoformat_basic_format_trim=1
let g:neoformat_python_black = {
      \ 'exe': 'black',
      \ 'stdin': 1,
      \ 'args': ['-q', '-', '-l 79'],
      \ }
let g:neoformat_enabled_python = ['black']
let g:neoformat_enabled_javascript = ['prettier']
let g:neoformat_enabled_htmldjango = ['prettier']
let g:neoformat_enabled_html = ['prettier']
let g:shfmt_opt="-ci"

" fzf
command! -bang -nargs=? -complete=dir Files
      \ call fzf#vim#files(
      \ <q-args>, {'options': ['--layout=reverse', '--info=inline', '--preview',
      \ '~/.config/nvim/plugged/fzf.vim/bin/preview.sh {}']}, <bang>0)

command! -bang -nargs=* Rg
      \ call fzf#vim#grep(
      \   'rg --column --line-number --no-heading --color=always --smart-case '.shellescape(<q-args>), 1,
      \   fzf#vim#with_preview(), <bang>0)

" #####################################################
" GENERAL CONFIG                                      #
" #####################################################

syntax off  " No colors
filetype plugin indent on

let mapleader=','  " Leader key

" Indentation
set expandtab
set shiftwidth=2
set tabstop=2
set softtabstop=2
autocmd FileType make   setlocal ts=8 sw=8 noexpandtab
autocmd FileType go     setlocal ts=8 sw=8 noexpandtab
autocmd FileType python setlocal ts=4 sw=4 sts=4
autocmd FileType perl   setlocal ts=4 sw=4 sts=4

" Nginx
au BufRead,BufNewFile */nginx/*.conf    set ft=nginx
au BufRead,BufNewFile */nginx/**/*.conf set ft=nginx

" Yaml
au BufNewFile,BufReadPost *.{yaml,yml} set filetype=yaml foldmethod=indent
au FileType yaml setlocal ts=2 sts=2 sw=2 expandtab

set encoding=utf-8
set fileencoding=utf-8
set updatetime=100      " async updatetime
set hidden              " hide buffer instead of closing them
set cmdheight=1         " height of cmd line
set nomodeline          " ignore vim modelines
set autoread            " reread changed files automatically
set laststatus=2        " always show statusline
set noshowcmd           " don't show cmd in last line of screen
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
set wildignorecase      " ignore case whem completing filenames and directories
set noshowmode          " do not show vim mode (already in statusline)
set splitbelow          " for ex preview windows will appear at the bottom
set noshowmode          " don't show mode (aleady in statusline)
set inccommand=nosplit  " show replacements using search / replace
set nocursorcolumn      " don't highlight column
set nocursorline        " don't highlight line
set shortmess+=c        " silence msg completion menu
set nolist              " hide special characters
set diffopt+=vertical   " diff splits
set visualbell t_vb=    " deactivate bells and alerts
set showbreak=⤿\        " line break symbol
set fillchars=vert:┃
set listchars=tab:→\ ,eol:¬,extends:>,precedes:<
set matchpairs+=<:>
set clipboard+=unnamedplus

" Ignore files and folders
set wildignore=*.swp,*.bak
set wildignore+=*.pyc,*.class,*.cache,*.dll,*.DS_Store,*.rdb,*.db,*.sqlite
set wildignore+=*/__pycache__/*,*/venv/*,*/env/*

" No swp files / backups etc
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

" Folds
function! CustomFoldText()
  " Custom fold lines format
  let line = getline(v:foldstart)
  let folded_line_num = v:foldend - v:foldstart
  let line_text = substitute(line, '^"{\+', '', 'g')
  return '    ⤿ +  (' . folded_line_num . ' lines) ' . line_text
endfunction
set foldmethod=indent
set foldlevel=99
set foldtext=CustomFoldText()

" Netrw settings
let g:netrw_banner=0
let g:netrw_sort_sequence='[\/]$,*'
let g:netrw_localrmdir="rm -r"
autocmd FileType netrw setl bufhidden=delete  " delete netrw buffer

" Deactivate dbtext plugin error msg on sql completion
let g:loaded_sql_completion=0
let g:omni_sql_no_default_maps=1

" Save as root
command! WW :w !sudo tee % >/dev/null

" Source on save config
autocmd! BufWritePost ~/.config/nvim/init.vim source ~/.config/nvim/init.vim

" Disable automatic insertion of comment markers
set fo=cjql
autocmd FileType * setl fo-=o fo-=r

" close method preview window after completion is complete
autocmd InsertLeave,CompleteDone * if pumvisible() == 0 | pclose | endif

" load custom dict files
au FileType * execute 'setlocal dict+=~/.config/nvim/dict/'.&filetype.'.txt'
call deoplete#custom#source('dictionary', 'matchers', ['matcher_head'])
call deoplete#custom#source('dictionary', 'sorters', [])
call deoplete#custom#source('dictionary', 'min_pattern_length', 2)

" markdown
au BufRead,BufNewFile *.md set ft=markdown
au BufRead,BufNewFile *.md setlocal list

" Remove trailing whitespaces
function! TrimTrailingWS()
  if exists('b:noStripWhitespace')
    return
  endif
  if search('\s\+$', 'cnw')
    :%s/\s\+$//g
  endif
endfunction
autocmd BufWritePre * :call TrimTrailingWS()
autocmd FileType markdown let b:noStripWhitespace=1

" #####################################################
" KEYBINDING                                          #
" #####################################################

" Start of line
nmap B ^
" End of line
nmap E $
" Navigate brackets
nmap <tab> %
" align paragraph
nmap <leader>a =ip
" copy paragraph
nmap cp yap<S-}>p
" Remove search highlight
nmap <silent><leader><space> :nohlsearch<cr>
" Edit config file
nmap <leader>e :e! ~/.config/nvim/init.vim<cr>

" FZF show buffer list
nmap <leader>b :Buffers<CR>
" FZF search files
nmap <leader>f :Files<CR>
" FZF search lines in buffer
nmap <leader>l :BLines<CR>
" FZF ripgrep
nmap <leader>; :Rg<CR>

" cd vim into current buffer directory
nmap <silent><leader>cd :cd %:p:h<CR>
" delete current buffer
nmap <silent>;d :bp\|bd #<CR>
" write
nmap ;w :w<CR>
" quit
nmap ;q :q<CR>
" format file
nmap ;f :Neoformat<CR>

" Navigate window panels
nmap <C-h> <C-w>h
nmap <C-j> <C-w>j
nmap <C-k> <C-w>k
nmap <C-l> <C-w>l

" Case insensitive replace word (aka multiple cursors)
nmap <Leader>x /\<<C-R>=expand('<cword>')<CR>\>\C<CR>``cgn
nmap <Leader>X ?\<<C-R>=expand('<cword>')<CR>\>\C<CR>``cgN

" Resize splits with arrow keys
nmap <silent><up> :res +5<CR>
nmap <silent><down> :res -5<CR>
nmap <silent><left> :vertical resize-5<CR>
nmap <silent><right> :vertical resize+5<CR>

" Keep visual selection when reindenting
xmap > >gv
xmap < <gv

" tab completion
imap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
imap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

" Auto close matching pairs
imap { {}<ESC>i
imap ( ()<ESC>i
imap [ []<ESC>i
imap < <><ESC>i

" Auto close matching pairs multi line
imap {<CR> {<CR>}<Esc>ko<tab>
imap [<CR> [<CR>]<Esc>ko<tab>
imap (<CR> (<CR>)<Esc>ko<tab>

" #####################################################
" DESIGN / COLORS AND STUFF                           #
" #####################################################

" Colorize some stuff (using syntax off)
hi DiffAdd        ctermfg=83 ctermbg=NONE guibg=NONE guifg=#5fff5f
hi DiffChange     ctermfg=222 ctermbg=NONE guibg=NONE guifg=#ffd787
hi DiffText       ctermfg=165 ctermbg=NONE guibg=NONE guifg=#d700ff
hi DiffDelete     ctermfg=197 ctermbg=NONE guibg=NONE guifg=#ff005f
hi Folded         ctermfg=231 ctermbg=239 guifg=#ffffff guibg=#4e4e4e
hi MatchParen     ctermfg=237 ctermbg=200 guifg=#3a3a3a guibg=#ff00d7
hi SignColumn     ctermbg=NONE cterm=NONE guibg=NONE gui=NONE
hi LineNr         ctermfg=239 ctermbg=NONE guifg=#4e4e4e guibg=NONE
hi VertSplit      ctermfg=240 ctermbg=NONE cterm=NONE guifg=#585858 guibg=NONE gui=NONE
hi StatuslineNC   ctermfg=250 ctermbg=238 cterm=NONE guifg=#bcbcbc guibg=#444444 gui=NONE
hi Statusline     ctermfg=234 ctermbg=252 cterm=NONE guifg=#1c1c1c guibg=#d0d0d0 gui=NONE
hi ALEErrorSign   ctermfg=161 ctermbg=NONE guibg=NONE guifg=#d7005f
hi ALEWarningSign ctermfg=221 ctermbg=NONE guibg=NONE guifg=#ffd75f

" GUI mode
if (has("gui_running"))
  set linespace=0
  set fontligatures
  set guifont=Monaco:h13
  set guioptions-=mTrL  " remove all GUI widgets
  set gcr=a:blinkon0    " no blinking cursor
endif

" Italics
let &t_ZH="\e[3m"
let &t_ZR="\e[23m"

" Cursor mode
let &t_SI.="\e[6 q" "SI = INSERT mode
let &t_SR.="\e[4 q" "SR = REPLACE mode
let &t_EI.="\e[2 q" "EI = NORMAL mode (ELSE)

" Show git info in statusline
function! GitInfo()
  let git = fugitive#head()
  if git != ""
    return "(on ".fugitive#head().")  "
  else
    return ""
  endif
endfunction

" Statusline vim mode colors
hi NormalColor   guifg=Black guibg=#5fff5f ctermbg=83  ctermfg=0
hi InsertColor   guifg=Black guibg=#5fd7ff ctermbg=81  ctermfg=0
hi ReplaceColor  guifg=Black guibg=#d7af5f ctermbg=180 ctermfg=0
hi VisualColor   guifg=Black guibg=#ff8700 ctermbg=208 ctermfg=0
hi CommandColor  guifg=Black guibg=#ff5f87 ctermbg=204 ctermfg=0

" Statusline active
function! ActiveStatusLine()
  let statusline=" %n "
  let statusline.="%#NormalColor#%{(mode()=='n')?'\ N\ ':''}"
  let statusline.="%#InsertColor#%{(mode()=='i')?'\ I\ ':''}"
  let statusline.="%#ReplaceColor#%{(mode()=='R')?'\ R\ ':''}"
  let statusline.="%#VisualColor#%{(mode()=='v')?'\ V\ ':''}"
  let statusline.="%#CommandColor#%{(mode()=='c')?'\ C\ ':''}"
  let statusline.="\%*\ %t\ %{GitInfo()}\ %{LinterStatus()}"
  let statusline.="%{&modified?'\  (+)':''}"
  let statusline.="%{&readonly?'\  (ro)':''}"
  let statusline.="\ %=%-14.(%l,%c%)"
  let statusline.="\ %y %{strlen(&fenc)?&fenc:&enc} "
  return statusline
endfunction

" Statusline inactive
function! InactiveStatusLine()
  let statusline=" %n "
  let statusline.="\%*\ %t\ %{GitInfo()}\ %{LinterStatus()}"
  let statusline.="%{&modified?'\  (+)':''}"
  let statusline.="%{&readonly?'\  (ro)':''}"
  let statusline.="\ %=%-14.(%l,%c%)"
  let statusline.="\ %y %{strlen(&fenc)?&fenc:&enc} "
  return statusline
endfunction

" Set statusline
set statusline=%!ActiveStatusLine()

" Switch windows statusline
augroup status
  autocmd!
  autocmd WinEnter * setlocal statusline=%!ActiveStatusLine()
  autocmd WinLeave * setlocal statusline=%!InactiveStatusLine()
augroup END
