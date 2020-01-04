" File  : mapping.vim
" Author: Matthieu Petiteau <mpetiteau.pro@gmail.com>
" Date  : 04.01.2020

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
map <leader>e :e! ~/dotfiles/vim/vimrc<cr>
autocmd! bufwritepost ~/dotfiles/vim/vimrc source ~/dotfiles/vim/vimrc
