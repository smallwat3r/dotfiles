" Neovim theme
" ~~~~~~~~~~~~

" Change cursor based on modes
let &t_SI .= '\e[6 q' " INSERT mode
let &t_SR .= '\e[4 q' " REPLACE mode
let &t_EI .= '\e[2 q' " NORMAL mode or others

" Custom colors (overwriting current colorscheme)
function! DefaultColors() abort
  if &diff
    syntax off
  endif

  hi CursorLineNR ctermbg=NONE
  hi DiffAdd      ctermfg=255  ctermbg=64
  hi DiffChange   ctermfg=204  ctermbg=NONE cterm=NONE
  hi DiffDelete   ctermfg=9    ctermbg=NONE cterm=NONE
  hi DiffText     ctermfg=255  ctermbg=31
  hi Directory    ctermfg=202  ctermbg=NONE
  hi EndOfBuffer  ctermbg=NONE
  hi IncSearch    ctermfg=232  ctermbg=229  cterm=bold
  hi LineNr       ctermfg=239  ctermbg=NONE
  hi MatchParen   ctermfg=203  ctermbg=190
  hi NonText      ctermfg=202  ctermbg=NONE
  hi Normal       ctermbg=NONE
  hi Search       ctermfg=232  ctermbg=11   cterm=NONE
  hi SignColumn   ctermbg=NONE
  hi SpellBad     ctermfg=88   ctermbg=210
  hi SpellCap     ctermbg=159  ctermfg=17
  hi StatuslineNC ctermfg=242  ctermbg=NONE cterm=underline
  hi Todo         ctermbg=NONE ctermfg=120
  hi VertSplit    ctermfg=242  ctermbg=NONE cterm=NONE
  hi Visual       cterm=reverse

  " Custom statusline colors
  hi SLCommandColor  ctermbg=210 ctermfg=88
  hi SLInsertColor   ctermbg=157 ctermfg=22
  hi SLNormalColor   ctermbg=15  ctermfg=233
  hi SLReplaceColor  ctermbg=159 ctermfg=17
  hi SLTerminalColor ctermbg=230 ctermfg=136
  hi SLVisualColor   ctermbg=222 ctermfg=166
endfunction

augroup custom_colors
  au!
  au ColorScheme * call DefaultColors()
augroup END

try
  colo seoul256
catch /^Vim\%((\a\+)\)\=:E185/
  colo desert
endtry
