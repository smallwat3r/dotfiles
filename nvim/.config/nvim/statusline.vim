" Neovim statusline
" ~~~~~~~~~~~~~~~~~

let symbols = {
      \ 'bwdseparator': "|",
      \ 'fwdseparator': "|",
      \ 'linenumber': "",
      \ 'modified': "[+]",
      \ 'readonly': "[ro]",
      \ 'branch': "@"
      \ }

" Show git info in statusline (with fugitive)
function! GitInfo()
  try
    if fugitive#head() != ''
      return g:symbols.branch . fugitive#head()
    endif
  catch
    return ''
  endtry
  return ''
endfunction

" Manage statusline colors from vim mode
function! ColorMode()
  if (mode() =~# '\v(n|no)')
    return '%#SLNormalColor# %{mode()} '
  elseif (mode() ==# 'i')
    return '%#SLInsertColor# %{mode()} '
  elseif (mode() ==# 'R')
    return '%#SLReplaceColor# %{mode()} '
  elseif (mode() =~# '\v(v|V)') || (mode() == "\<C-v>")
    return '%#SLVisualColor# %{mode()} '
  elseif (mode() ==# 'c')
    return '%#SlCommandColor# %{mode()} '
  elseif (mode() ==# 't')
    return '%#SlTerminalColor# %{mode()} '
  endif
  return ''
endfunction

" Statusline format
function! StatusLineFmt(active)
  let sl = ''
  if a:active
    let sl .= ColorMode() . GitInfo()
  endif
  let sl .= ' %t '
  let sl .= ' %{&modified? g:symbols.modified:""}'
  let sl .= ' %{&readonly? g:symbols.readonly:""}'
  let sl .= ' %=%-14.(%{g:symbols.linenumber}%l,%c%)'
  let sl .= ' %{strlen(&fenc)?&fenc:&enc}'
  let sl .= ' %{&filetype}'
  let sl .= ' %n '
  return sl
endfunction

" Active and non-active on window change event
augroup status
  au!
  au WinEnter * setl statusline=%!StatusLineFmt(1)
  au WinLeave * setl statusline=%!StatusLineFmt(0)
augroup END

" Set statusline (1 = active by default)
set statusline=%!StatusLineFmt(1)
