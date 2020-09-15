" Neovim settings
" ~~~~~~~~~~~~~~~

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
set undodir=~/.config/nvim/undodir
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
  au BufNewFile,BufRead * setlocal formatoptions-=cro
augroup END
