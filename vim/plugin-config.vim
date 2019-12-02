" File  : plugin-config.vim
" Author: smallwat3r
" Date  : Wed  7 Aug 19:49:07 2019

" Plugins Configs

" vim git-gutter
let g:gitgutter_sign_added='▴'
let g:gitgutter_sign_modified='▴'
let g:gitgutter_sign_removed='▴'
hi GitGutterAdd    guifg=#009900 ctermfg=2
hi GitGutterChange guifg=#bbbb00 ctermfg=3
hi GitGutterDelete guifg=#ff2222 ctermfg=1

" FZF
set rtp+=/usr/local/opt/fzf
let g:fzf_layout={ 'down': '~20%' }
let g:fzf_action={
      \ 'ctrl-t': 'tab split',
      \ 'ctrl-x': 'split',
      \ 'ctrl-v': 'vsplit' }
let g:fzf_colors={
      \ 'fg':      ['fg', 'Normal'],
      \ 'bg':      ['bg', 'Normal'],
      \ 'hl':      ['fg', 'SpecialKey'],
      \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
      \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
      \ 'hl+':     ['fg', 'SpecialKey'],
      \ 'info':    ['fg', 'PreProc'],
      \ 'border':  ['fg', 'Ignore'],
      \ 'prompt':  ['fg', 'Conditional'],
      \ 'pointer': ['fg', 'Exception'],
      \ 'marker':  ['fg', 'Keyword'],
      \ 'spinner': ['fg', 'Label'],
      \ 'header':  ['fg', 'Comment'] }

" Sort Python imports on save
" let g:impsort_relative_last=1
" autocmd BufWritePre *.py ImpSort!

" Auto Headers on F5
let g:header_auto_add_header=0
let g:header_field_author='Matthieu Petiteau'
let g:header_field_author_email='mpetiteau.pro@gmail.com'
let g:header_field_timestamp_format='%d.%m.%Y'
let g:header_field_modified_timestamp=0
let g:header_field_modified_by=0
map <F5> :AddHeader<CR>

" Ale
let g:ale_echo_msg_error_str = 'E'
let g:ale_echo_msg_warning_str = 'W'
let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'
let g:ale_pattern_options = {
      \ '\.min\.js$': {'ale_linters': [], 'ale_fixers': []},
      \ '\.min\.css$': {'ale_linters': [], 'ale_fixers': []},
      \ }

" Lightline
" let g:shades_of_purple_lightline=1
" let g:lightline = {
"       \ 'colorscheme': 'shades_of_purple',
"       \ 'active': {
"       \   'left': [ [ 'mode', 'paste' ],
"       \             [ 'gitbranch', 'readonly', 'filename', 'modified' ] ]
"       \ },
"       \ 'component_function': {
"       \   'gitbranch': 'fugitive#head'
"       \ },
"       \ }

" Markdown
let g:vim_markdown_folding_disabled=1
