" File              : keybindings.vim
" Author            : smallwat3r
" Date              : Wed  7 Aug 19:48:53 2019

" Mapping and keybindings

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
