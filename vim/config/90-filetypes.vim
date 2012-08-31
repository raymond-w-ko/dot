" C {{{
augroup ft_c
  autocmd!
  autocmd BufNewFile,BufRead *.c setlocal foldnestmax=1
augroup END
" }}}
" C# {{{
augroup ft_cs
  autocmd!
  autocmd BufNewFile,BufRead *.cs setlocal foldnestmax=1 foldmethod=indent
augroup END
" }}}
" C++ {{{
augroup ft_cpp
  autocmd!
  autocmd BufNewFile,BufRead *.cpp,*.cc setlocal foldnestmax=1
augroup END
" }}}
" Objective-C {{{
augroup ft_objc
  autocmd!
  autocmd BufNewFile,BufRead *.m setlocal foldnestmax=1
augroup END
" }}}
" C, C++, Obj-C Header {{{
augroup ft_h
  autocmd!
  autocmd BufNewFile,BufRead *.h setlocal foldnestmax=20
augroup END
" }}}

" Syandus Spec File {{{
augroup ft_ssf
  autocmd!
  autocmd BufNewFile,BufRead *.ssf setlocal foldnestmax=20
augroup END
" }}}
" SyML {{{
augroup ft_sml
  autocmd!
  autocmd BufNewFile,BufRead *.sml setlocal foldnestmax=20
augroup END
" }}}
" HLSL, FX, FXL {{{
augroup ft_fx
  autocmd!
  autocmd BufNewFile,BufRead *.fx,*.fxl,*.fxh,*.hlsl setlocal filetype=fx foldnestmax=20
augroup END
" }}}

" Java {{{
augroup ft_java
    au!
    au FileType java setlocal foldmethod=marker foldmarker={,}
augroup END
" }}}
" Javascript {{{
augroup ft_javascript
    au!
    au FileType javascript setlocal foldmethod=marker foldmarker={,}
augroup END
" }}}
" vimrc {{{
augroup vimrc
  au!
  au BufWritePost _vimrc source $MYVIMRC
  au BufWritePost */vimfiles/config/* source $MYVIMRC
augroup END
" }}}
" git {{{
augroup ft_gitcommit
  au!
  au FileType gitcommit setlocal foldlevel=9001
augroup END
"}}}
" Python {{{
augroup ft_python
  au!
  au BufNewFile,BufRead *.py setlocal foldmethod=syntax foldlevel=1
  au BufNewFile,BufRead *.py setlocal omnifunc=pythoncomplete#Complete
augroup END
"}}}
" autohotkey {{{
augroup ft_autohotkey
  au!
  au FileType autohotkey setlocal foldlevel=0 foldnestmax=1 foldmethod=marker
augroup END
"}}}
" DOS Batch Files {{{
augroup ft_dosbatch
  au!
  au FileType dosbatch setlocal ff=dos
augroup END
"}}}

" Hex Editing {{{
" vim -b : edit binary using xxd-format!
augroup Binary
  au!

  " set binary option for all binary files before reading them
  "au BufReadPre *.bin,*.hex,*.exe,*.dll setlocal binary

  " if on a fresh read the buffer variable is already set, it's wrong
  "au BufReadPost *
  "\ if exists('b:editHex') && b:editHex |
  "\   let b:editHex = 0 |
  "\ endif

  " convert to hex on startup for binary files automatically
  "au BufReadPost *
  "\ if &binary | HexMode | endif

  " When the text is freed, the next time the buffer is made active it will
  " re-read the text and thus not match the correct mode, we will need to
  " convert it again if the buffer is again loaded.
  "au BufUnload *
  "\ if getbufvar(expand("<afile>"), 'editHex') == 1 |
  "\   call setbufvar(expand("<afile>"), 'editHex', 0) |
  "\ endif

  " before writing a file when editing in hex mode, convert back to non-hex
  "au BufWritePre *
  "\ if exists("b:editHex") && b:editHex && &binary |
  "\  let oldro=&ro | let &ro=0 |
  "\  let oldma=&ma | let &ma=1 |
  "\  silent exe "%!xxd -r" |
  "\  let &ma=oldma | let &ro=oldro |
  "\  unlet oldma | unlet oldro |
  "\ endif

  " after writing a binary file, if we're in hex mode, restore hex mode
  "au BufWritePost *
  "\ if exists("b:editHex") && b:editHex && &binary |
  "\  let oldro=&ro | let &ro=0 |
  "\  let oldma=&ma | let &ma=1 |
  "\  silent exe "%!xxd" |
  "\  silent exe "%:s/$//g" |
  "\  exe "set nomod" |
  "\  let &ma=oldma | let &ro=oldro |
  "\  unlet oldma | unlet oldro |
  "\ endif
augroup END
" }}}
" vim:fdm=marker:foldlevel=0
