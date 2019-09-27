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

fun! LinterStatus() abort
  let l:counts = ale#statusline#Count(bufnr(''))
  let l:all_errors = l:counts.error + l:counts.style_error
  let l:all_non_errors = l:counts.total - l:all_errors
  return l:counts.total == 0 ? 'OK' : printf(
        \   '%dW %dE',
        \   all_non_errors,
        \   all_errors
        \)
endfun
