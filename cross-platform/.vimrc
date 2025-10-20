" smallwat3r's vim config

"{{{1 General Settings

syntax on
filetype plugin indent on

let mapleader = ','  " Leader key

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
" set nu rnu              " relative line numbers and current line number
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
set nocursorcolumn      " don't highlight column
set nocursorline        " don't highlight line
set shortmess+=c        " silence message completion menu
set nolist              " hide special characters
set diffopt+=vertical   " diff splits
set visualbell t_vb=    " deactivate bells and alerts
set showbreak=⤿\        " line break symbol
set foldmethod=marker   " use marker to fold lines
set synmaxcol=500       " keep long lines from slowing vim
set fillchars=vert:┃
set listchars=tab:→\ ,eol:¬,extends:>,precedes:<,nbsp:˷,trail:␣
set matchpairs+=<:>
set clipboard+=unnamedplus
set spellfile=~/.config/nvim/spell/en.utf-8.add
set backspace=indent,eol,start
set whichwrap+=<,>,h,l

if exists('+autochdir')
  set autochdir
else
  autocmd BufEnter * silent! lcd %:p:h:gs/ /\\ /
endif

if has('nvim')
  set inccommand=nosplit  " show replacements using search / replace
endif

set wildignore=*.swp,*.bak,*.pyc,*.class,*.cache,*.dll,*.DS_Store,*.rdb,*.db,*.sqlite
set wildignore+=__pycache__/*,venv/*,env/*,.git/*,build/*,node_modules/*,dist/*

" Undo and backups
set noswapfile
set nobackup
set directory=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set undolevels=4000
set undoreload=100000
set undodir=~/.vim/undodir
set undofile

" Indentation
set expandtab
set shiftwidth=2
set tabstop=2
set softtabstop=2

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

" Filetypes
augroup filetype_detect
  au!
  au BufRead,BufNewFile *.md                      setf markdown
  au BufRead,BufNewFile */nginx/*.conf            setf nginx
  au BufRead,BufNewFile */nginx/**/*.conf         setf nginx
  au BufRead,BufNewFile *.{yaml,yml}              setf yaml
  au BufRead,BufNewFile gitconfig                 setf gitconfig
  au BufRead,BufNewFile *.sketch                  setf sketch
  au BufRead,BufNewFile Dockerfile.*              setf dockerfile
  au BufRead,BufNewFile *.{dockerfile,Dockerfile} setf dockerfile
  au BufRead,BufNewFile *.{applescript,osascript} setf applescript
  au BufRead,BufNewFile *.log                     setf log
  au BufRead,BufNewFile *.html                    setf jinja
augroup END

augroup filetype_indentation
  au!
  au FileType make   setl ts=8 sw=8 noet
  au FileType go     setl ts=8 sw=8 noet
  au FileType python setl ts=4 sw=4 sts=4 et
  au FileType perl   setl ts=4 sw=4 sts=4 et
augroup END

augroup filetype_specifics
  au!
  au FileType gitcommit setl spell
  au FileType markdown  setl spell list cc=70 | let b:noStripWhitespace=1
  au FileType sketch    setl spell
  au FileType netrw     setl bh=delete
  au FileType text      setl cc=70
augroup END

" Netrw
let g:netrw_banner        = 0
let g:netrw_sort_sequence = '[\/]$,*'
let g:netrw_localrmdir    = 'rm -r'

" Deactivate db text plugin error msg on sql completion
let g:loaded_sql_completion    = 0
let g:omni_sql_no_default_maps = 1

" Fix weird Python indent behaviour
let g:pyindent_open_paren   = '0'
let g:pyindent_nested_paren = '&sw'
let g:pyindent_continue     = '&sw'

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
  au FileType * setlocal formatoptions-=cro
augroup END

"}}}1 General Settings
"{{{1 Plugins

" Auto load for first time use - Install Vim Plug Manager
if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
        \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  au VimEnter * PlugInstall --sync | source $MYVIMRC
endif

nmap <leader>pl :source $MYVIMRC<cr>:PlugInstall<cr>
nmap <leader>pc :source $MYVIMRC<cr>:PlugClean<cr>
nmap <leader>pu :source $MYVIMRC<cr>:PlugUpdate<cr>

call plug#begin()

Plug 'jiangmiao/auto-pairs'            " Auto close pairs
Plug 'junegunn/vim-easy-align'         " Align pieces of text
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

Plug 'machakann/vim-sandwich'          " Surroundings mapping
Plug 'sbdchd/neoformat'                " Auto code formatting
let g:neoformat_basic_format_align = 1
let g:neoformat_basic_format_retab = 1
let g:neoformat_basic_format_trim  = 1

let g:neoformat_javascript_prettier = {
      \ 'exe': 'prettier',
      \ 'stdin': 1,
      \ 'args': [
      \   '--stdin',
      \   '--config ~/.config/.prettierrc',
      \   '--stdin-filepath',
      \   '"%:p"'
      \   ],
      \ }

let g:neoformat_enabled_python     = ['yapf', 'black']
let g:neoformat_enabled_javascript = ['prettier']
let g:neoformat_enabled_zsh        = ['shfmt']
let g:shfmt_opt = '-ci'

nmap ;f :Neoformat<cr>

Plug 'tpope/vim-commentary'            " Comments mappings
Plug 'tpope/vim-fugitive'              " Git wrapper
nnoremap <leader>gd :Gvdiffsplit!<cr>
nnoremap gdh :diffget //2<cr>
nnoremap gdl :diffget //3<cr>

Plug 'cocopon/vaffle.vim'              " File explorer
nmap <silent>- :Vaffle<cr>

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

let g:vaffle_show_hidden_files = 1
let g:vaffle_force_delete = 1

Plug 'Vimjas/vim-python-pep8-indent'   " Better python indentation

" Fuzzy finder
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

let g:fzf_colors =
      \ { 'fg':      ['fg', 'Normal'],
      \ 'bg':      ['bg', 'Normal'],
      \ 'hl':      ['fg', 'String'],
      \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
      \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
      \ 'hl+':     ['fg', 'Statement'],
      \ 'info':    ['fg', 'PreProc'],
      \ 'border':  ['fg', 'Ignore'],
      \ 'prompt':  ['fg', 'Conditional'],
      \ 'pointer': ['fg', 'Exception'],
      \ 'marker':  ['fg', 'Keyword'],
      \ 'spinner': ['fg', 'Label'],
      \ 'header':  ['fg', 'Comment'] }

let g:fzf_layout = { 'window': { 'width': 0.9, 'height': 0.6, 'highlight': 'Normal' } }

nmap <leader>b  :Buffers<cr>
nmap <leader>f  :GFiles<cr>
nmap <leader>;  :Rg<cr>
nmap <leader>co :Commits<cr>
nmap <leader>l  :BLines<cr>
nmap <leader>w; :exe ":Rg     " . expand('<cword>')<cr>
nmap <leader>wl :exe ":BLines " . expand('<cword>')<cr>

call plug#end()

"}}}1 Plugins
"{{{1 Bindings

"{{{2 normal mode

" center search results
nnoremap n  nzz
nnoremap N  Nzz
nnoremap *  *zz
nnoremap #  #zz
nnoremap g* g*zz
nnoremap g# g#zz

" toggle scrolloff center
nnoremap <Leader>zz :let &scrolloff=999-&scrolloff<CR>

" navigate to end and start of line
nmap B ^
nmap E $

" join to previous line
nmap <silent><C-k> :-join<CR>

" navigate between brackets
nmap <tab> %

" go to last change
nmap <leader><leader> `.

" align paragraph
nmap <leader>a =ip

" delete to blackhole register (don't lose previous yank)
nmap s  "_d
nmap ss "_dd

" remove search highlight
map <silent><leader><space> :nohlsearch<cr>

" cd into current buffer directory
nmap <silent><leader>cd :cd %:p:h<cr>:pwd<cr>

" cd into previous directory
nmap <silent><leader>cdp :cd ..<cr>:pwd<cr>

" delete current buffer
nmap <silent><space>bd :bp\|bd #<cr>:echo 'Buffer deleted'<cr>

" quick write and exit
nmap ;w :w<cr>
nmap ;q :q<cr>

" splits
nmap <space>ws :sp<cr>
nmap <space>wv :vs<cr>

" navigate window panels
nmap <space>wh <C-w>h
nmap <space>wj <C-w>j
nmap <space>wk <C-w>k
nmap <space>wl <C-w>l

" resize splits
nmap <silent><S-k> :res +5<cr>
nmap <silent><S-j> :res -5<cr>
nmap <silent><S-h> :vertical resize-5<cr>
nmap <silent><S-l> :vertical resize+5<cr>

" play macros
nmap Q @q

" run Python linters
augroup python_linters
  au!
  au Filetype python nmap <leader>py :! pylint %<cr>
  au Filetype python nmap <leader>is :! isort %<cr>
augroup END

"}}}2 normal mode
"{{{2 visual mode

" play macros with visual mode
vmap Q :norm @q<cr>

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

" jk works as esc
imap jk <esc>

" crtl + hjkl cursor movement on insert mode
imap <C-h> <left>
imap <C-j> <down>
imap <C-k> <up>
imap <C-l> <right>

"}}}2 insert mode
"{{{2 insert mode abbreviations

" Annoying words I don't know how to write
iab widht    width
iab lenght   length
iab strenght strength
iab weigth   weight

"}}}2 insert mode abbreviations

"}}}1 Bindings
"{{{1 StatusLine

let symbols = {
      \ 'bwdseparator': "|",
      \ 'fwdseparator': "|",
      \ 'linenumber': "",
      \ 'modified': "[+]",
      \ 'readonly': "[ro]",
      \ 'branch': "@"
      \ }

" Show git info in statusline (with fugitive)
function! GitInfo()
  try
    if fugitive#head() != ''
      return g:symbols.branch . fugitive#head()
    endif
  catch
    return ''
  endtry
  return ''
endfunction

function! StatuslineColors() abort
  hi SLCommandColor  ctermbg=210 ctermfg=88
  hi SLInsertColor   ctermbg=157 ctermfg=22
  hi SLNormalColor   ctermbg=15  ctermfg=233
  hi SLReplaceColor  ctermbg=159 ctermfg=17
  hi SLTerminalColor ctermbg=230 ctermfg=136
  hi SLVisualColor   ctermbg=222 ctermfg=166
endfunction

augroup statusline_colors
  au!
  au ColorScheme * call StatuslineColors()
augroup END

" Manage statusline colors from vim mode
function! ColorMode()
  if (mode() =~# '\v(n|no)')
    return '%#SLNormalColor# %{mode()} '
  elseif (mode() ==# 'i')
    return '%#SLInsertColor# %{mode()} '
  elseif (mode() ==# 'R')
    return '%#SLReplaceColor# %{mode()} '
  elseif (mode() =~# '\v(v|V)') || (mode() == "\<C-v>")
    return '%#SLVisualColor# %{mode()} '
  elseif (mode() ==# 'c')
    return '%#SlCommandColor# %{mode()} '
  elseif (mode() ==# 't')
    return '%#SlTerminalColor# %{mode()} '
  endif
  return ''
endfunction

" Statusline format
function! StatusLineFmt(active)
  let sl = ''
  if a:active
    let sl .= ColorMode() . GitInfo()
  endif
  let sl .= ' %t '
  let sl .= ' %{&modified? g:symbols.modified:""}'
  let sl .= ' %{&readonly? g:symbols.readonly:""}'
  let sl .= ' %=%-14.(%{g:symbols.linenumber}%l,%c%)'
  let sl .= ' %{strlen(&fenc)?&fenc:&enc}'
  let sl .= ' %{&filetype}'
  let sl .= ' %n '
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

"}}}1 StatusLine
"{{{1 Theme

" Change cursor based on modes
let &t_SI .= '\e[6 q' " INSERT mode
let &t_SR .= '\e[4 q' " REPLACE mode
let &t_EI .= '\e[2 q' " NORMAL mode or others

function! DefaultColors() abort
  if &diff
    syntax off
  endif

  hi CursorLineNR ctermbg=NONE
  hi Comment      cterm=italic
  hi DiffAdd      ctermfg=255  ctermbg=64
  hi DiffChange   ctermfg=204  ctermbg=NONE cterm=NONE
  hi DiffDelete   ctermfg=9    ctermbg=NONE cterm=NONE
  hi DiffText     ctermfg=255  ctermbg=31
  hi Directory    ctermbg=NONE
  hi EndOfBuffer  ctermbg=NONE
  hi IncSearch    ctermfg=232  ctermbg=229  cterm=bold
  hi LineNr       ctermfg=239  ctermbg=NONE
  hi MatchParen   ctermfg=203  ctermbg=190
  hi NonText      ctermfg=202  ctermbg=NONE
  hi Normal       ctermbg=NONE
  hi Pmenu        ctermfg=242  ctermbg=235
  hi PmenuSel     ctermfg=254  ctermbg=242
  hi Search       ctermfg=232  ctermbg=11   cterm=NONE
  hi SignColumn   ctermbg=NONE
  hi SpellBad     ctermfg=88   ctermbg=210
  hi SpellCap     ctermbg=159  ctermfg=17
  hi StatuslineNC ctermfg=242  ctermbg=NONE cterm=underline
  hi Todo         ctermbg=NONE ctermfg=120
  hi VertSplit    ctermfg=242  ctermbg=NONE cterm=NONE
  hi Visual       cterm=reverse
endfunction

try
  colo vividchalk
catch /^Vim\%((\a\+)\)\=:E185/
  colo elflord
  augroup custom_colors
    au!
    au ColorScheme * call DefaultColors()
  augroup END
endtry

"}}}1 Theme
