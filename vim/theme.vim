" File  : theme.vim
" Author: Matthieu Petiteau <mpetiteau.pro@gmail.com>
" Date  : 04.01.2020

" ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" Theme and custo
" ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

" Colors
set t_Co=256
set bg=dark
colo simplicity

" Gui settings
if (has("gui_running"))
    set linespace=0
    set macligatures
    set guifont=Andale_Mono_SwFix:h14
    " set transparency=5
    set guioptions-=mTrL  " remove all GUI widgets
    " set gcr=a:blinkon0    " no blinking cursor
endif

" All the below must be set after colorschemes
" --------------------------------------------
" Statusline
set statusline=%{fugitive#statusline()}
set statusline+=\ %f
set statusline+=\ %{strlen(&ft)?&ft:'none'}
set statusline+=\ %{LinterStatus()}
set statusline+=\ %=
set statusline+=\ %l/%L\ c%c
set statusline+=\ %{strlen(&fenc)?&fenc:&enc}\ |

" Italics
let &t_ZH="\e[3m"
let &t_ZR="\e[23m"
