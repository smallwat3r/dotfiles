" File  : plugins.vim
" Author: Matthieu Petiteau <mpetiteau.pro@gmail.com>
" Date  : 04.01.2020

" ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" Plugins
" ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

" Using vim-plug
call plug#begin()

Plug 'dense-analysis/ale'
Plug 'jiangmiao/auto-pairs'
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
let g:header_field_author='Matthieu Petiteau'
let g:header_field_author_email='mpetiteau.pro@gmail.com'
let g:header_field_timestamp_format='%d.%m.%Y'
let g:header_auto_add_header=0
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
let b:ale_linters = ['flake8']
let b:ale_fixers = ['black']

" Impsort - Python imports sorting on save.
autocmd BufWritePre *.py ImpSort!
