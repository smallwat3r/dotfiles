" File  : init.vim
" Author: Matthieu Petiteau <mpetiteau.pro@gmail.com>
" Date  : 05.02.2020
"
" neovim config file
"

"
" PLUGIN MANAGER
" --------------------------------------------------------------------
" Using vim-plug
call plug#begin()

Plug 'dense-analysis/ale'
Plug 'jiangmiao/auto-pairs'
Plug 'sbdchd/neoformat'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-vinegar'
Plug 'dhruvasagar/vim-table-mode'
Plug 'alvan/vim-closetag'
Plug 'gregsexton/MatchTag'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'Vimjas/vim-python-pep8-indent'
Plug 'machakann/vim-sandwich'
Plug 'itchyny/vim-highlighturl'
Plug 'pacha/vem-tabline'
Plug 'tpope/vim-unimpaired'

" Completion
Plug 'davidhalter/jedi-vim'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'deoplete-plugins/deoplete-jedi'
Plug 'Shougo/context_filetype.vim'
Plug 'Shougo/neopairs.vim'
Plug 'Shougo/neoinclude.vim'

call plug#end()

"
" PLUGINS CONFIG
" --------------------------------------------------------------------

" use deoplete
let g:deoplete#enable_at_startup=1
let g:deoplete#sources#jedi#show_docstring=1

" vim git-gutter
let g:gitgutter_sign_added='+'
let g:gitgutter_sign_modified='~'
let g:gitgutter_sign_removed='-'

" Ale
let g:ale_echo_msg_error_str='E'
let g:ale_echo_msg_warning_str='W'
let g:ale_set_highlights=0
let g:ale_sign_error='>>'
let g:ale_sign_warning='--'
let g:ale_echo_msg_format='[%linter%] %s [%severity%]'

" neoformat
let g:neoformat_basic_format_align=1
let g:neoformat_basic_format_retab=1
let g:neoformat_basic_format_trim=1

" fzf
command! -bang -nargs=? -complete=dir Files
    \ call fzf#vim#files(<q-args>, {'options': ['--layout=reverse', '--info=inline', '--preview', '~/.vim/plugged/fzf.vim/bin/preview.sh {}']}, <bang>0)

command! -bang -nargs=* Rg
  \ call fzf#vim#grep(
  \   'rg --column --line-number --no-heading --color=always --smart-case '.shellescape(<q-args>), 1,
  \   fzf#vim#with_preview(), <bang>0)

" Vem tabline
let g:vem_tabline_show_number='buffnr'

"
" GENERAL BEHAVIOUR
" --------------------------------------------------------------------
syntax on

filetype plugin indent on

" Remap leader
let mapleader=','

" Default indentation
set expandtab
set shiftwidth=4
set tabstop=4

" Indentation other filetypes
autocmd FileType make   setlocal ts=8 sw=8 noexpandtab
autocmd FileType go     setlocal ts=8 sw=8 noexpandtab

" Encodings
setglobal termencoding=utf-8 fileencodings=
scriptencoding utf-8
set encoding=utf8

set nomodeline
set autoread  " reread changed files automatically
set ffs=unix
set ttyfast
set laststatus=2  " always show statusline
set noshowcmd
set noruler
set modifiable
set showmatch  " matching brackets
set mouse=a  " mouse support
set nostartofline
set incsearch  " search pattern
set hlsearch  " search highlighting
set clipboard=unnamedplus
set wrap  " wrap lines
set lazyredraw  " no redraw
set ignorecase  " search ignore case
set scrolljump=8  " minimal nb of lines to scroll when cursor gets off the screen
set autochdir  " auto change working directory
set nonu  " hide row numbers
set fillchars=vert:┃
set nocompatible " modern vim
set showmode  " show vim mode (insert, visual, replace)
set wildignorecase
set matchpairs+=<:>
set splitbelow  " for ex preview windows will appear at the bottom
set noshowmode " don't show mode (aleady in statusline)

" md filetype
autocmd BufNewFile,BufFilePre,BufRead *.md set filetype=markdown

set nolist  " hide special characters
au BufNewFile,BufFilePre,BufRead *.md setlocal list  " but activate on md files

" diff splits
set diffopt+=vertical

" Special chars
set showbreak=⤿\     " wrap lines symbol
set listchars=tab:→\ ,eol:¬,extends:>,precedes:<

" Folding
set foldmethod=indent
set foldlevel=99

" Ignore files and folders
set wildignore=*.swp,*.bak
set wildignore+=*.pyc,*.class,*.cache,*.dll,*.DS_Store,*.rdb
set wildignore+=*/__pycache__/*,*/venv/*

" Deactivate bells and alerts
set tm=500

" No swp files / backups etc
set noswapfile
set nobackup

" Backspace as it should work
set backspace=indent,eol,start
set whichwrap+=<,>,h,l

" Netrw settings
let g:netrw_banner=0
let g:netrw_sort_sequence='[\/]$,*'
let g:netrw_localrmdir="rm -r"
autocmd FileType netrw setl bufhidden=delete  " delete netrw buffer

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

" access pydocs
nmap <buffer> H :<C-u>execute "!pydoc3 " . expand("<cword>")<CR>

" close method preview window after completion is complete
autocmd InsertLeave,CompleteDone * if pumvisible() == 0 | pclose | endif

"
" THEME
" --------------------------------------------------------------------
set background=dark
set t_Co=256

colo smallwat3r

" Other colortheme for Markdown
autocmd! BufEnter,BufNewFile *.md colo elflord
autocmd! BufLeave *.md colo smallwat3r

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

" Statusline
function! GitInfo()
  let git = fugitive#head()
  if git != ''
    return '⎧'.fugitive#head().'⎫'
  else
    return ''
endfunction

" Statusline vim mode colors
hi NormalColor guifg=Black guibg=Green ctermbg=46 ctermfg=0
hi InsertColor guifg=Black guibg=Cyan ctermbg=51 ctermfg=0
hi ReplaceColor guifg=Black guibg=maroon1 ctermbg=165 ctermfg=0
hi VisualColor guifg=Black guibg=Orange ctermbg=202 ctermfg=0
hi CommandColor guifg=Black guibg=Pink ctermbg=13 ctermfg=0

" Statusline active
function! ActiveStatusLine()
    let statusline="⎧%n⎫"
    let statusline.="%#NormalColor#%{(mode()=='n')?'\ NORMAL\ ':''}"
    let statusline.="%#InsertColor#%{(mode()=='i')?'\ INSERT\ ':''}"
    let statusline.="%#ReplaceColor#%{(mode()=='R')?'\ REPLACE\ ':''}"
    let statusline.="%#VisualColor#%{(mode()=='v')?'\ VISUAL\ ':''}"
    let statusline.="%#CommandColor#%{(mode()=='c')?'\ COMMAND\ ':''}"
    let statusline.="\%*\ %<%F\ %{GitInfo()}\ %{LinterStatus()}"
    let statusline.="%{&modified?'\  ⎧+⎫':''}"
    let statusline.="%{&readonly?'\  ⎧RO⎫':''}"
    let statusline.="\ %=%-14.(%l,%c%)"
    let statusline.="\ %{strlen(&fenc)?&fenc:&enc}\ %P\ %L "
    return statusline
endfunction

" Statusline inactive
function! InactiveStatusLine()
    let statusline="⎧%n⎫"
    let statusline.="\%*\ %<%F\ %{GitInfo()}\ %{LinterStatus()}"
    let statusline.="%{&modified?'\  ⎧+⎫':''}"
    let statusline.="%{&readonly?'\  ⎧RO⎫':''}"
    let statusline.="\ %=%-14.(%l,%c%)"
    let statusline.="\ %{strlen(&fenc)?&fenc:&enc}\ %P\ %L "
    return statusline
endfunction

set statusline=%!ActiveStatusLine()

" Switch windows statusline
augroup status
    autocmd!
    autocmd WinEnter * setlocal statusline=%!ActiveStatusLine()
    autocmd WinLeave * setlocal statusline=%!InactiveStatusLine()
augroup END

"
" KEYBINDINGS
" --------------------------------------------------------------------
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

" Remove search highlight
nmap <leader><space> :nohlsearch<cr>

" Editing and reloading of config
map <leader>e :e! ~/dotfiles/vim/init.vim<cr>
autocmd! bufwritepost ~/dotfiles/vim/init.vim source ~/dotfiles/vim/init.vim

" Navigate files, buffers etc. (fzf)
nmap <leader>b :Buffers<CR>
nmap <leader>f :Files<CR>
nmap <leader>l :BLines<CR>
nmap <leader>; :Rg<CR>

" Case insensitive replace word (aka multiple cursors)
nmap <Leader>x /\<<C-R>=expand('<cword>')<CR>\>\C<CR>``cgn
nmap <Leader>X ?\<<C-R>=expand('<cword>')<CR>\>\C<CR>``cgN

" cd vim into current buffer directory
nmap <silent><leader>cd :cd %:p:h<CR>

" save current file
nmap <silent>;; :w<CR>

" delete current buffer, keep window layout
nmap <silent>;d :bp\|bd #<CR>

" improve pop up completion menu
inoremap <silent><expr><tab> pumvisible() ? "\<c-n>" : "\<tab>"
inoremap <silent><expr><s-tab> pumvisible() ? "\<c-p>" : "\<s-tab>"

" FUNCTIONS
" --------------------------------------------------------------------

" Remove trailing whitespaces
function! TrimTrailingWS ()
    if exists('b:noStripWhitespace')
        return
    endif
    if search('\s\+$', 'cnw')
        :%s/\s\+$//g
    endif
endfunction

autocmd BufWritePre * :call TrimTrailingWS()
autocmd FileType markdown let b:noStripWhitespace=1

" Count errors in status bar.
fun! LinterStatus() abort
    let l:counts=ale#statusline#Count(bufnr(''))
    let l:all_errors=l:counts.error + l:counts.style_error
    let l:all_non_errors=l:counts.total - l:all_errors
    return l:counts.total == 0 ? 'OK' : printf(
                \   '%dW %dE',
                \   all_non_errors,
                \   all_errors )
endfun

" Custom fold lines format
function! CustomFoldText()
    let line = getline(v:foldstart)
    let folded_line_num = v:foldend - v:foldstart
    let line_text = substitute(line, '^"{\+', '', 'g')
    return '    ⤿ +  (' . folded_line_num . ' lines) ' . line_text
endfunction

set foldtext=CustomFoldText()
