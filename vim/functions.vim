" File              : functions.vim
" Author            : smallwat3r
" Date              : Thu  8 Aug 20:10:13 2019

" Custom functions

" Remove trailing whitespaces.
fun! StripTrailingWhitespaces()
  if exists('b:noStripWhitespace')
    return
  endif
  let _s=@/
  let l=line(".")
  let c=col(".")
  %s/\s\+$//e
  let @/=_s
  call cursor(l, c)
endfun
autocmd BufWritePre * :call StripTrailingWhitespaces()
autocmd FileType markdown let b:noStripWhitespace=1
