if !filereadable('/dev/clipboard')
    finish
endif

function! SetClipboard(type, ...) range
  let sel_save = &selection
  let &selection = "inclusive"
  let reg_save = @@
  if a:type == 'n'
    silent exe a:firstline . "," . a:lastline . "y"
  elseif a:type == 'c'
    silent exe a:1 . "," . a:2 . "y"
  else
    silent exe "normal! `<" . a:type . "`>y"
  endif
  "call system('putclip', @@)
  "As of Cygwin 1.7.13, the /dev/clipboard device was added to provide
  "access to the native Windows clipboard. It provides the added benefit
  "of supporting utf-8 characters which putclip currently does not. Based
  "on a tip from John Beckett, use the following:
  call writefile(split(@@,"\n"), '/dev/clipboard', 'b')
  let &selection = sel_save
  let @@ = reg_save
endfunction

function! GetClipboard()
  let reg_save = @@
  "let @@ = system('getclip')
  "Much like Putclip(), using the /dev/clipboard device to access to the
  "native Windows clipboard for Cygwin 1.7.13 and above. It provides the
  "added benefit of supporting utf-8 characters which getclip currently does
  "not. Based again on a tip from John Beckett, use the following:
  let @@ = join(readfile('/dev/clipboard'), "\n")
  setlocal paste
  exe 'normal p'
  setlocal nopaste
  let @@ = reg_save
endfunction

" Since keys are laid out like [Cut] [Copy] [Paste] for the left hand
" Mirrored is [Paste] [Copy] [Cut], for [F9] [10] [F11] [F12]
"nnoremap <silent> <F9>  :call GetClipboard()<CR>
"vnoremap <silent> <F10> :call SetClipboard(visualmode(), 1)<CR>
"nnoremap <silent> <F10> :call SetClipboard('n', 1)<CR>
