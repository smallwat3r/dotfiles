" Neovim mappings / keybindings
" ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

"{{{1 normal mode

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
nmap <silent>`j :-join<CR>

" navigate between brackets
nmap <tab> %

" go to last change
nmap <leader><leader> `.

" align paragraph
nmap <leader>a =ip

" copy paragraph
nmap cp yap<S-}>p

" delete to blackhole register (don't lose previous yank)
nmap s  "_d
nmap ss "_dd

" remove search highlight
map <silent><leader><space> :nohlsearch<cr>

" edit config file
nmap <leader>e :e! $MYVIMRC<cr>

" source init.vim
nmap <silent><leader>so :source $MYVIMRC<cr>:echo 'config sourced'<cr>

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

" run Python linters
augroup python_linters
  au!
  au Filetype python nmap <leader>py :! pylint %<cr>
  au Filetype python nmap <leader>is :! isort %<cr>
augroup END

"}}}1 normal mode
"{{{1 visual mode

" remap easier access to visual blocks
nnoremap <C-v> v
nnoremap v <C-v>

" play macros with visual mode
vmap Q :norm @q<cr>

" keep visual selection when re-indenting
xmap > >gv
xmap < <gv

" vaa select the entire file
xmap aa VGo1G

"}}}1 visual mode
"{{{1 insert mode

" tab completion
imap <expr> <tab>   pumvisible() ? '<c-n>' : '<tab>'
imap <expr> <s-tab> pumvisible() ? '<c-p>' : '<s-tab>'

" jk works as esc
imap jk <esc>

" hjkl insert go-to (new line or end/start of line)
imap 1h <esc>I
imap 1j <esc>o
imap 1k <esc>O
imap 1l <esc>A

" crtl + hjkl cursor movement on insert mode
imap <C-h> <left>
imap <C-j> <down>
imap <C-k> <up>
imap <C-l> <right>

" Delete a word
imap <C-d> <ESC>ciw

"}}}1 insert mode
"{{{1 command mode

" crtl + hjkl cursor movement on command mode
cmap <C-h> <left>
cmap <C-j> <down>
cmap <C-k> <up>
cmap <C-l> <right>

"}}}1 command mode
"{{{1 insert mode abbreviations

" Personal stuff
iab @e,  mpetiteau.pro@gmail.com
iab @me, Matthieu Petiteau <mpetiteau.pro@gmail.com>
iab @c,  Copyright 2020 Matthieu Petiteau, all rights reserved.

" Shebang
iab @s,  #!/usr/bin/env
iab @sh, #!/usr/bin/env bash
iab @pe, #!/usr/bin/env perl
iab @py, #!/usr/bin/env python3

" Current date / datetime / timestamp ISO8601/W3C
iab @d,  <C-R>=strftime("%a, %d %b %Y")<cr>
iab @dt, <C-R>=strftime("%a, %d %b %Y at %H:%M")<cr>
iab @ts, <C-R>=strftime("%FT%T%z")<cr>

" Line separators
iab @-, -----------------------------------------------------------------
iab @#, #################################################################
iab @*, *****************************************************************

" Annoying words I don't know how to write
iab widht    width
iab lenght   length
iab strenght strength
iab weigth   weight

"}}}1 insert mode abbreviations
