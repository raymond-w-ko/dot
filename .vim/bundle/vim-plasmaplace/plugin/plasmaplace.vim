" plasmaplace.vim - Clojure REPL support
if exists("g:loaded_plasmaplace") || v:version < 800 || &compatible
  finish
endif
let g:loaded_plasmaplace = 1

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" utils
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" create a unique key based on the full path of the current buffer
function! s:get_project_key() abort
  let path = expand("%:p:h")
  let prev_path = path
  while 1
    if filereadable(path."/project.clj") || filereadable(path."./deps.edn")
      break
    endif
    let prev_path = path
    let path = fnamemodify(path, ":h") 
    if path == prev_path
      throw "plasmaplace: could not determine project directory"
    endif
  endwhile
  echom path
endfunction

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" vars
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let s:repl_jobs = {}

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" main
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! s:K() abort
  call s:get_project_key()
  return "" 
endfunction

nnoremap <Plug>PlasmaplaceK :<C-R>=<SID>K()<CR><CR>

function! s:setup_keybinds() abort
  nmap <buffer> K <Plug>PlasmaplaceK
endfunction

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

augroup plasmaplace_keybinds
  autocmd!
  autocmd FileType clojure call s:setup_keybinds()
augroup END
