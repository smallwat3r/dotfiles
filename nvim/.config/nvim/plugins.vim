" Neovim plugins

" Auto load for first time use - Install Vim Plug Manager
if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
        \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  au VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin()

Plug 'alvan/vim-closetag'              " Auto-close html tags
Plug 'christoomey/vim-tmux-navigator'  " Tmux navigation
Plug 'gregsexton/MatchTag'             " Highlight matching html tag
Plug 'jiangmiao/auto-pairs'            " Auto close pairs
Plug 'junegunn/gv.vim'                 " Git commit browser
Plug 'junegunn/vim-easy-align'         " Align pieces of text
Plug 'machakann/vim-sandwich'          " Surroundings mapping
Plug 'sbdchd/neoformat'                " Auto code formatting
Plug 'tpope/vim-commentary'            " Comments mappings
Plug 'tpope/vim-eunuch'                " Shell commands from vim
Plug 'tpope/vim-fugitive'              " Git wrapper
Plug 'tpope/vim-unimpaired'            " Complementary mappings
Plug 'tpope/vim-vividchalk'            " Colorscheme
Plug 'vifm/vifm.vim'                   " File manager
Plug 'Vimjas/vim-python-pep8-indent'   " Fix python indentation behaviour
Plug 'zirrostig/vim-schlepp'           " Move visual blocks

" Syntax support
Plug 'chr4/nginx.vim'
Plug 'ekalinin/Dockerfile.vim'
Plug 'Glench/Vim-Jinja2-Syntax'
Plug 'vim-scripts/applescript.vim'

" Fuzzy finder
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

" Completion
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'deoplete-plugins/deoplete-dictionary'
Plug 'deoplete-plugins/deoplete-jedi'

call plug#end()

" Plugin configurations
" ~~~~~~~~~~~~~~~~~~~~~

"{{{1 deoplete

let g:deoplete#enable_at_startup = 1
let g:deoplete#sources#jedi#enable_typeinfo = 0

" Dictionaries
call deoplete#custom#source('dictionary', 'matchers', ['matcher_head'])
call deoplete#custom#source('dictionary', 'sorters', [])
call deoplete#custom#source('dictionary', 'min_pattern_length', 2)

augroup filetype_dictionaries
  au!
  au Filetype python,sql,javascript,html,go,dockerfile,css
        \ execute 'setl dict+=~/.config/nvim/dict/' . &filetype . '.txt'
augroup END

"}}}1 deoplete
"{{{1 neoformat

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
let g:neoformat_html_prettier = {
      \ 'exe': 'prettier',
      \ 'stdin': 1,
      \ 'args': [
      \   '--stdin',
      \   '--config ~/.config/.prettierrc',
      \   '--stdin-filepath',
      \   '"%:p"'
      \   ],
      \ }
let g:neoformat_jinja_prettier = {
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
let g:neoformat_enabled_html       = ['prettier']
let g:neoformat_enabled_jinja      = ['prettier']
let g:neoformat_enabled_zsh        = ['shfmt']
let g:shfmt_opt = '-ci'

" format file
nmap ;f :Neoformat<cr>

"}}}1 neoformat
"{{{1 fzf

let g:fzf_layout = {
      \ 'window': {
      \   'width': 0.9,
      \   'height': 0.6,
      \   'highlight': 'Todo',
      \   'border': 'sharp'
      \ }}

com! -bang -nargs=? -complete=dir Files
      \ call fzf#vim#files(
      \ <q-args>, {
      \   'options': [
      \     '--layout=reverse',
      \     '--info=inline',
      \     '--preview',
      \     '~/.config/nvim/plugged/fzf.vim/bin/preview.sh {}'
      \   ]
      \ }, <bang>0)

com! -bang -nargs=* Rg
      \ call fzf#vim#grep(
      \   'rg --column --line-number --no-heading --color=always --smart-case '
      \   .shellescape(<q-args>), 1, fzf#vim#with_preview(), <bang>0)

nmap <leader>b  :Buffers<cr>
nmap <leader>f  :Files<cr>
nmap <leader>;  :Rg<cr>
nmap <leader>w; :exe ":Rg     " . expand('<cword>')<cr>
nmap <leader>co :Commits<cr>
nmap <leader>l  :BLines<cr>
nmap <leader>wl :exe ":BLines " . expand('<cword>')<cr>

"}}}1 fzf
"{{{1 vim sandwich

let g:sandwich#recipes = deepcopy(g:sandwich#default_recipes)
let g:sandwich#recipes += [
      \   {
      \     'buns'      : ['{', '}'],
      \     'motionwise': ['line'],
      \     'kind'      : ['add'],
      \     'linewise'  : 1,
      \     'command'   : ["'[+1,']-1normal! >>"],
      \   },
      \   {
      \     'buns'      : ['{', '}'],
      \     'motionwise': ['line'],
      \     'kind'      : ['delete'],
      \     'linewise'  : 1,
      \     'command'   : ["'[,']normal! <<"],
      \   }
      \ ]

"}}}1 vim sandwich
"{{{1 easy-align

" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

"}}}1 easy-align
"{{{1 vifm

" Trigger Vifm with -
nnoremap <silent>- :execute 'Vifm ' . ((strlen(bufname('')) == 0) ? '.' : '%:h')<CR>

"}}}1 vifm
"{{{1 fugitive

" Fugitive conflict resolution
nnoremap <leader>gd :Gvdiffsplit!<cr>
nnoremap gdh :diffget //2<cr>
nnoremap gdl :diffget //3<cr>

"}}}1 fugitive
"{{{1 gv

" git logs current file
nmap <leader>gl :GV!<cr>

"}}}1 Gv
"{{{1 vim-plug

" vim-plug
nmap <leader>pl :source $MYVIMRC<cr>:PlugInstall<cr>
nmap <leader>pc :source $MYVIMRC<cr>:PlugClean<cr>
nmap <leader>pu :source $MYVIMRC<cr>:PlugUpdate<cr>

"}}}1 vim-plug
"{{{1 vim-schlepp

" crtl + hjkl move visual blocks
vmap <C-h> <Plug>SchleppLeft
vmap <C-j> <Plug>SchleppDown
vmap <C-k> <Plug>SchleppUp
vmap <C-l> <Plug>SchleppRight

"}}}1 vim-schlepp
