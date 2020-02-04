" Author: Matthieu Petiteau <mpetiteau.pro@gmail.com>
" A super minimalist vim colorscheme that I often change...

hi clear

if exists("syntax on")
  syntax reset
endif

let g:colors_name="smallwat3r"
set background=dark

"
" Colors
"
hi Normal              ctermfg=231  ctermbg=NONE cterm=NONE       guifg=#ffffff  guibg=#000000  gui=NONE
hi Visual              ctermfg=233  ctermbg=229  cterm=NONE       guifg=#121212  guibg=#ffffaf  gui=NONE

hi Comment             ctermfg=158  ctermbg=NONE cterm=NONE       guifg=#afff87  guibg=NONE     gui=NONE
hi LineNr              ctermfg=239  ctermbg=NONE cterm=NONE       guifg=#4e4e4e  guibg=NONE     gui=NONE

hi NonText             ctermfg=241  ctermbg=NONE cterm=NONE       guifg=#626262  guibg=NONE     gui=NONE

hi Statusline          ctermfg=235  ctermbg=231  cterm=NONE       guifg=#262626  guibg=#ffffff  gui=NONE
hi StatuslineNC        ctermfg=233  ctermbg=240  cterm=NONE       guifg=#121212  guibg=#585858  gui=NONE

hi ColorColumn         ctermfg=NONE ctermbg=NONE cterm=NONE       guifg=NONE     guibg=NONE     gui=NONE

hi Cursor              ctermfg=234  ctermbg=51                    guifg=#262626  guibg=#00ffff
hi CursorColumn        ctermfg=250  ctermbg=NONE cterm=NONE       guifg=#b9b9b9  guibg=NONE     gui=NONE
hi CursorLine          ctermfg=250  ctermbg=NONE cterm=NONE       guifg=#b9b9b9  guibg=NONE     gui=NONE
hi CursorLineNR        ctermfg=250  ctermbg=NONE cterm=NONE       guifg=#b9b9b9  guibg=NONE     gui=NONE

hi DiffAdd             ctermfg=46  ctermbg=NONE guibg=NONE guifg=#00ff00
hi DiffChange          ctermfg=222 ctermbg=NONE guibg=NONE guifg=#ffd787
hi DiffText            ctermfg=165 ctermbg=NONE guibg=NONE guifg=#d700ff
hi DiffDelete          ctermfg=197 ctermbg=NONE guibg=NONE guifg=#ff005f

hi GitGutterAdd        ctermfg=154  ctermbg=NONE cterm=NONE       guifg=#afff00  guibg=NONE     gui=NONE
hi GitGutterChange     ctermfg=229  ctermbg=NONE cterm=NONE       guifg=#ffffaf  guibg=NONE     gui=NONE
hi GitGutterDelete     ctermfg=197  ctermbg=NONE cterm=NONE       guifg=#ff005f  guibg=NONE     gui=NONE

hi SpellBad     ctermfg=9   ctermbg=NONE cterm=underline  guifg=#ff0000 guibg=NONE gui=underline
hi SpellCap     ctermfg=50  ctermbg=NONE cterm=underline  guifg=#00ffd7 guibg=NONE gui=underline
hi SpellRare    ctermfg=128 ctermbg=NONE cterm=underline  guifg=#af00d7 guibg=NONE gui=underline
hi SpellLocal   ctermfg=123 ctermbg=NONE cterm=underline  guifg=#87ffff guibg=NONE gui=underline

hi VertSplit    ctermfg=240  ctermbg=NONE cterm=NONE       guifg=#585858  guibg=NONE     gui=NONE
hi SignColumn   ctermfg=235  ctermbg=NONE cterm=NONE       guifg=#262626  guibg=NONE     gui=NONE

hi MatchParen   ctermfg=237  ctermbg=200 guifg=#3a3a3a  guibg=#ff00d7
hi MatchWord    ctermfg=200 guifg=#ff00d7 cterm=italic gui=italic

hi Search       ctermfg=163 ctermbg=NONE cterm=bold           guifg=#d700af guibg=NONE gui=bold
hi IncSearch    ctermfg=163 ctermbg=NONE cterm=bold,underline guifg=#d700af guibg=NONE gui=bold,underline

hi TODO                ctermfg=190  ctermbg=NONE cterm=NONE       guifg=#d7ff00  guibg=NONE     gui=NONE
hi Error               ctermfg=9    ctermbg=NONE cterm=underline  guifg=#ff0000  guibg=NONE     gui=underline
hi ErrorMsg            ctermfg=9    ctermbg=NONE cterm=underline  guifg=#ff0000  guibg=NONE     gui=underline

hi Directory           ctermfg=200 ctermbg=NONE cterm=NONE       guifg=#ff00d7  guibg=NONE     gui=NONE
hi netrwDir            ctermfg=200 ctermbg=NONE cterm=NONE       guifg=#ff00d7  guibg=NONE     gui=NONE

" hi Pmenu               ctermfg=241  ctermbg=237  cterm=NONE       guifg=#626262  guibg=#3a3a3a  gui=NONE
" hi PmenuSbar           ctermfg=241  ctermbg=237  cterm=NONE       guifg=#626262  guibg=#3a3a3a  gui=NONE
" hi PmenuThumb          ctermfg=241  ctermbg=237  cterm=NONE       guifg=#626262  guibg=#3a3a3a  gui=NONE
" hi PmenuSel            ctermfg=230  ctermbg=237  cterm=NONE       guifg=#ffffd7  guibg=#3a3a3a  gui=NONE

hi Folded              ctermfg=231  ctermbg=239  cterm=bold       guifg=#ffffff  guibg=#4e4e4e  gui=bold
hi FoldColumn          ctermfg=231  ctermbg=239  cterm=bold       guifg=#ffffff  guibg=#4e4e4e  gui=bold

hi Title               ctermfg=NONE ctermbg=NONE cterm=NONE       guifg=NONE     guibg=NONE     gui=NONE
hi Constant            ctermfg=NONE ctermbg=NONE cterm=NONE       guifg=NONE     guibg=NONE     gui=NONE
hi Character           ctermfg=NONE ctermbg=NONE cterm=NONE       guifg=NONE     guibg=NONE     gui=NONE
hi String              ctermfg=NONE ctermbg=NONE cterm=NONE       guifg=NONE     guibg=NONE     gui=NONE
hi Number              ctermfg=NONE ctermbg=NONE cterm=NONE       guifg=NONE     guibg=NONE     gui=NONE
hi Float               ctermfg=NONE ctermbg=NONE cterm=NONE       guifg=NONE     guibg=NONE     gui=NONE
hi Boolean             ctermfg=NONE ctermbg=NONE cterm=NONE       guifg=NONE     guibg=NONE     gui=NONE
hi Identifier          ctermfg=NONE ctermbg=NONE cterm=NONE       guifg=NONE     guibg=NONE     gui=NONE
hi Include             ctermfg=NONE ctermbg=NONE cterm=NONE       guifg=NONE     guibg=NONE     gui=NONE
hi Function            ctermfg=NONE ctermbg=NONE cterm=NONE       guifg=NONE     guibg=NONE     gui=NONE
hi Statement           ctermfg=NONE ctermbg=NONE cterm=NONE       guifg=NONE     guibg=NONE     gui=NONE
hi Conditional         ctermfg=NONE ctermbg=NONE cterm=NONE       guifg=NONE     guibg=NONE     gui=NONE
hi Operator            ctermfg=NONE ctermbg=NONE cterm=NONE       guifg=NONE     guibg=NONE     gui=NONE
hi Repeat              ctermfg=NONE ctermbg=NONE cterm=NONE       guifg=NONE     guibg=NONE     gui=NONE
hi Type                ctermfg=NONE ctermbg=NONE cterm=NONE       guifg=NONE     guibg=NONE     gui=NONE
hi Character           ctermfg=NONE ctermbg=NONE cterm=NONE       guifg=NONE     guibg=NONE     gui=NONE
hi Special             ctermfg=NONE ctermbg=NONE cterm=NONE       guifg=NONE     guibg=NONE     gui=NONE
hi SpecialChar         ctermfg=NONE ctermbg=NONE cterm=NONE       guifg=NONE     guibg=NONE     gui=NONE
hi SpecialKey          ctermfg=NONE ctermbg=NONE cterm=NONE       guifg=NONE     guibg=NONE     gui=NONE
hi PreProc             ctermfg=NONE ctermbg=NONE cterm=NONE       guifg=NONE     guibg=NONE     gui=NONE
hi Conceal             ctermfg=NONE ctermbg=NONE cterm=NONE       guifg=NONE     guibg=NONE     gui=NONE
hi Debug               ctermfg=NONE ctermbg=NONE cterm=NONE       guifg=NONE     guibg=NONE     gui=NONE
hi Define              ctermfg=NONE ctermbg=NONE cterm=NONE       guifg=NONE     guibg=NONE     gui=NONE
hi Delimiter           ctermfg=NONE ctermbg=NONE cterm=NONE       guifg=NONE     guibg=NONE     gui=NONE
hi Directive           ctermfg=NONE ctermbg=NONE cterm=NONE       guifg=NONE     guibg=NONE     gui=NONE
hi Exception           ctermfg=NONE ctermbg=NONE cterm=NONE       guifg=NONE     guibg=NONE     gui=NONE
hi Format              ctermfg=NONE ctermbg=NONE cterm=NONE       guifg=NONE     guibg=NONE     gui=NONE
hi Ignore              ctermfg=NONE ctermbg=NONE cterm=NONE       guifg=NONE     guibg=NONE     gui=NONE
hi Keyword             ctermfg=NONE ctermbg=NONE cterm=NONE       guifg=NONE     guibg=NONE     gui=NONE
hi Label               ctermfg=NONE ctermbg=NONE cterm=NONE       guifg=NONE     guibg=NONE     gui=NONE
hi Macro               ctermfg=NONE ctermbg=NONE cterm=NONE       guifg=NONE     guibg=NONE     gui=NONE
hi PreCondit           ctermfg=NONE ctermbg=NONE cterm=NONE       guifg=NONE     guibg=NONE     gui=NONE
hi SpecialComment      ctermfg=NONE ctermbg=NONE cterm=NONE       guifg=NONE     guibg=NONE     gui=NONE
hi StorageClass        ctermfg=NONE ctermbg=NONE cterm=NONE       guifg=NONE     guibg=NONE     gui=NONE
hi Structure           ctermfg=NONE ctermbg=NONE cterm=NONE       guifg=NONE     guibg=NONE     gui=NONE
hi Tag                 ctermfg=NONE ctermbg=NONE cterm=NONE       guifg=NONE     guibg=NONE     gui=NONE
hi Typedef             ctermfg=NONE ctermbg=NONE cterm=NONE       guifg=NONE     guibg=NONE     gui=NONE
hi Underlined          ctermfg=NONE ctermbg=NONE cterm=NONE       guifg=NONE     guibg=NONE     gui=NONE
hi Tooltip             ctermfg=NONE ctermbg=NONE cterm=NONE       guifg=NONE     guibg=NONE     gui=NONE
hi Menu                ctermfg=NONE ctermbg=NONE cterm=NONE       guifg=NONE     guibg=NONE     gui=NONE
