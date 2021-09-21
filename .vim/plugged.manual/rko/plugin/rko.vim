command! StripTrailingWhitespace call rko#strip_trailing_whitespace()
if has('unix')
  nnoremap <leader>m :update<CR>:call rko#find_and_run_makefile()<CR>
endif

" converts underscore_case to camelCase
" nnoremap <leader>c :s#_\(\l\)#\u\1#<CR>
" vnoremap <leader>c :s#_\(\l\)#\u\1#<CR>

augroup plugin_rko
  au!

  " Use map <buffer> to only map dd in the quickfix window. Requires +localmap
  au FileType qf map <buffer> dd :call rko#remove_qf_item<cr>
  au FileType qf map <buffer> q :cclose<cr>
  au BufReadPost quickfix nnoremap <buffer> <CR> :call rko#open_qf_item()<cr>
augroup END

