" escape pathname with spaces so it doesn't break other commands and functions
fun! rko#escape_pathname(pathname) abort
  return substitute(a:pathname, "\\ ", "\\\\ ", "g")
endf

fun! rko#toggle_linter() abort
  if g:ale_echo_delay <= 10
    " OFF
    echom "ALE popup off"
    let g:ale_echo_delay = 100000 
    if has("nvim")
      let g:ale_hover_to_floating_preview=0
      let g:ale_detail_to_floating_preview=0
      let g:ale_floating_preview=0
      let g:ale_cursor_detail=0
    endif
  else
    " ON
    echom "ALE popup on"
    let g:ale_echo_delay = 0
    if has("nvim")
      let g:ale_hover_to_floating_preview=1
      let g:ale_detail_to_floating_preview=1
      let g:ale_floating_preview=1
      let g:ale_cursor_detail=1
    endif
  endif
endfun

fun! rko#strip_trailing_whitespace() abort
  let l:my_saved_winview = winsaveview()
  silent! %s/\s\+$//
  call winrestview(l:my_saved_winview)
endf

fun! rko#left_brace() abort
  let line_number = line('.')
  let starting_line_number = line_number
  let line_number -= 1

  while line_number >= 1
    if match(getline(line_number), '^\s*$') != -1
      break
    endif
    let line_number -= 1
  endwhile

  if line_number != starting_line_number && line_number != 0
    exe 'normal! ' . line_number . 'G'
  elseif line_number == 0
    normal! 1G
  else
    return
  endif

  normal! 0

  return
endf

fun! rko#right_brace() abort
  let line_number = line('.')
  let starting_line_number = line_number
  let line_number += 1

  let max_bounds = line('$')

  while line_number <= max_bounds
    if (match(getline(line_number), '^\s*$') != -1)
      break
    endif
    let line_number += 1
  endwhile

  if line_number != starting_line_number && line_number <= max_bounds
    exe 'normal! ' . line_number . 'G'
  elseif line_number > max_bounds
    normal! G
  else
    return
  endif

  normal! 0

  return
endf

let s:list_of_pairs = [
    \ ['(', ')'],
    \ ['[', ']'],
    \ ['{', '}'],
    \ ['"', '"'],
    \ ["'", "'"],
    \ ]
fun! rko#empty_pair_bs()
  let line = getline('.')
  let n = strlen(line)
  let pos = col('.')
  if pos <= 1 || pos > n
    return "\<BS>"
  endif

  let left = line[pos-2]
  let right = line[pos-1]

  for pairs in s:list_of_pairs
    if left == pairs[0] && right == pairs[1]
      return "\<C-g>U\<Right>\<BS>\<BS>"
    endif
  endfor

  return "\<BS>"
endf

fun! rko#smart_cr()
  let keys = ""
  if pumvisible()
    let keys .= "\<C-e>"
  endif

  let line = getline('.')
  let n = strlen(line)
  let pos = col('.')
  if pos <= 1 || pos > n
    return keys . "\<CR>"
  endif

  let left = line[pos-2]
  let right = line[pos-1]

  for pairs in s:list_of_pairs
    if left == pairs[0] && right == pairs[1]
      return keys . "\<CR>\<Esc>O"
    endif
  endfor
  return keys . "\<CR>"
endf

fun! rko#basic_cr()
  let keys = ""
  if pumvisible()
    let keys .= "\<C-e>"
  endif
  return keys . "\<CR>"
endf

let s:move_right_keystroke = "\<C-g>U\<Right>"
let s:move_right_pair_ends = { "'" : 1, '"' : 1, ')' : 1, ']' : 1, '}' : 1 }
fun! rko#paredit_forward_up() abort
  let keys = ''
  if pumvisible()
    let keys .= "\<C-y>"
  endif

  let line = getline('.')
  let n = strlen(line)
  let steps_right = 0
  let x = col('.') - 1
  if n > 0
    while x < n
      let ch = line[x]
      if has_key(s:move_right_pair_ends, ch)
        break
      endif

      let steps_right += 1
      let x += 1
    endwhile
  endif

  if n == 0 || x == n
    " do nothing
  else
    let keys .= repeat(s:move_right_keystroke, steps_right+1)
  endif

  return keys
endf

fun! rko#setup_pair_bindings()
  " handled by vim-sexp
  if &ft == 'clojure' || &ft == 'lisp' || &ft == 'scheme'
    exe "imap <silent><buffer> φ ("
    exe "imap <silent><buffer> σ {"
    exe "imap <silent><buffer> ρ ["
    exe 'imap <silent><buffer> θ "'
    inoremap <silent><buffer> <CR> <C-r>=rko#basic_cr()<CR>
    inoremap <silent><buffer> <BS> <C-r>=rko#empty_pair_bs()<CR>
  else
    " semimap helpers
    inoremap <silent><buffer> φ ()<C-g>U<Left>
    inoremap <silent><buffer> σ {}<C-g>U<Left>
    inoremap <silent><buffer> ρ []<C-g>U<Left>
    inoremap <silent><buffer> θ ""<C-g>U<Left>
    inoremap <silent><buffer> υ <><C-g>U<Left>
    inoremap <silent><buffer> <CR> <C-r>=rko#smart_cr()<CR>
"
    if &filetype != "clojure"
      inoremap <silent><buffer> ( ()<C-g>U<Left>
      inoremap <silent><buffer> { {}<C-g>U<Left>
      inoremap <silent><buffer> [ []<C-g>U<Left>
      inoremap <silent><buffer> " ""<C-g>U<Left>
      inoremap <silent><buffer> <CR> <C-r>=rko#smart_cr()<CR>
      inoremap <silent><buffer> <BS> <C-r>=rko#empty_pair_bs()<CR>
    endif
  endif
endf

fun! rko#is_project_directory(directory) abort
  if isdirectory(a:directory . "/.git")
    return 1
  elseif isdirectory(a:directory . "/.hg")
    return 1
  elseif filereadable(a:directory . "/shadow-cljs.edn")
    return 1
  else
    return 0
  endif
endf

" traverse up parent directories until it finds one that matches in the above
" list
fun! rko#get_project_directory() abort
  let last_directory = ''
  let directory = expand("%:p:h")

  while !rko#is_project_directory(directory) && last_directory != directory
    let last_directory = directory
    let directory = substitute(simplify(directory . '/..'),
        \ '[\\/]*$', '', '')
  endwhile

  if last_directory == directory
    " we could not find a project directory
    return getcwd()
  elseif has('win32')
    return directory . '\'
  else
    return directory . '/'
endf

fun! rko#show_syntax_groups() abort
  call feedkeys("\<Plug>ScripteaseSynnames")
  TSHighlightCapturesUnderCursor
endf

fun! rko#center_cursor() abort
  if winheight(0) < 25 | return | endif
  normal! zz
endf

" use aesthetic middle of screen for "zz"
fun! rko#center_cursor_aesthetically() abort
  normal! zz

  let center = round(winheight(0) / 2.0)
  let offset = winheight(0) * 0.1
  let final = center - offset
  let rounded_final = float2nr(final)
  let rounded_offset = float2nr(offset)
  let delta = winline() - (rounded_final + 1)

  if delta > 0
    exe 'normal ' . delta . "\<C-e>"
  endif
endf


fun! rko#setup_help_tab()
  if &buftype == 'help'
    " silent wincmd T
    nnoremap <buffer> q :q<cr>
  endif
endf

" command to delete all empty buffers in case you have over 9000 of them
fun! rko#delete_empty_buffers() abort
  let empty = []
  let [i, nbuf] = [1, bufnr('$')]
  while i <= nbuf
      if bufexists(i) && bufname(i) == ''
          let empty += [i]
      endif
      let i += 1
  endwhile
  if len(empty) > 0
      execute 'bdelete ' . join(empty, ' ')
  endif
endf

fun! rko#find_and_run_makefile()
  let prev_dir = ''
  let current_dir = expand('%:p:h')

  let max_search = 0

  while current_dir != prev_dir && current_dir != '/'
    let max_search += 1

    let makefile = current_dir . '/Makefile'
    if filereadable(makefile)
      let make_cmd = "make -f Makefile " . '-C ' . current_dir
      noautocmd belowright split
      resize 25
      exe "term " . make_cmd
      nnoremap <buffer> q :q<cr>
      return
    endif

    let prev_dir = current_dir
    let current_dir = simplify(current_dir . '/..')
    if max_search == 8
      break
    endif
  endwhile
endf

" When using `dd` in the quickfix list, remove the item from the quickfix list.
fun! rko#remove_qf_item()
  let curqfidx = line('.') - 1
  let qfall = getqflist()
  call remove(qfall, curqfidx)
  call setqflist(qfall, 'r')
endf

fun! rko#open_qf_item()
  let idx = line('.') - 1
  let items = getqflist()
  let item = items[idx]
  echom item
  wincmd k
  wincmd l
  exe "b " item["bufnr"]
  exe "normal " . item["lnum"] . "G"
endfun

fun! rko#format_java()
  let view = winsaveview()
  execute "%!google-java-format --skip-removing-unused-imports -"
  call winrestview(view)
endf
fun! rko#format_json()
  let view = winsaveview()
  execute "%!python -m json.tool"
  call winrestview(view)
endf
fun! rko#format_js()
  let view = winsaveview()
  execute "%!prettier --parser babel --trailing-comma es5"
  call winrestview(view)
endf
fun! rko#format_html()
  let view = winsaveview()
  execute "%!prettier --parser html"
  call winrestview(view)
endf
fun! rko#format_scss()
  let view = winsaveview()
  execute "%!prettier --parser scss"
  call winrestview(view)
endf
fun! rko#format_css()
  let view = winsaveview()
  execute "%!prettier --parser css"
  call winrestview(view)
endf
fun! rko#format_python()
  let view = winsaveview()
  execute "%!black -q -"
  call winrestview(view)
endf
fun! rko#format_golang()
  let view = winsaveview()
  execute "%!gofmt"
  call winrestview(view)
endf

fun! rko#mark_window_swap()
  let g:markedWinNum = winnr()
endf
fun! rko#do_window_swap()
  "Mark destination
  let curNum = winnr()
  let curBuf = bufnr( "%" )
  exe g:markedWinNum . "wincmd w"
  "Switch to source and shuffle dest->source
  let markedBuf = bufnr( "%" )
  "Hide and open so that we aren't prompted and keep history
  exe 'hide buf' curBuf
  "Switch to dest and shuffle source->dest
  exe curNum . "wincmd w"
  "Hide and open so that we aren't prompted and keep history
  exe 'hide buf' markedBuf
endf

fun! rko#create_vsplits()
  let num_tabs=tabpagenr("$")
  if num_tabs == 1
    if winnr("$") > 1
      tabnew
    endif
  else
      tabnew
  endif
  
  let num_vsplits = (&columns / (80 - 1)) - 1

  " create number of vsplits based off of argument passwd
  for i in range(num_vsplits)
    vnew
  endfor

  " move back to left vsplit
  for i in range(num_vsplits)
    wincmd h
  endfor

  wincmd =
endf
