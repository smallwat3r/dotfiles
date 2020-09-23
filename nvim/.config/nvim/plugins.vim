" Neovim plugins
" ~~~~~~~~~~~~~~

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

Plug 'junegunn/seoul256.vim'           " Colorscheme

Plug 'christoomey/vim-tmux-navigator'  " Tmux navigation
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

" Plug 'vifm/vifm.vim'                   " File manager (commented out as using Vaffle as a file explorer)
"   nnoremap <silent>- :execute 'Vifm ' . ((strlen(bufname('')) == 0) ? '.' : '%:h')<CR>

Plug 'Vimjas/vim-python-pep8-indent'   " Better python indentation

" Fuzzy finder
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
let $FZF_DEFAULT_OPTS .= ' --inline-info'

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

" Completion
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'deoplete-plugins/deoplete-jedi'
let g:deoplete#enable_at_startup = 1
let g:deoplete#sources#jedi#enable_typeinfo = 0

call plug#end()
