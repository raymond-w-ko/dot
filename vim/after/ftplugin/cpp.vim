function! MyCppFunctionBeginJump(dir)
  let line_num = line('.') + a:dir
  while (line_num > 0 && line_num <= line('$'))
    let line = getline(line_num)
    if line =~ '^{\s*' || line =~ '^\S.\+{\s*$'
      let list = [bufnr('%'), line_num, col('.'), 0]
      call setpos('.', list)
      return
    endif
    let line_num = line_num + a:dir
  endwhile
endfunction

nnoremap <silent> <buffer> [[ :call MyCppFunctionBeginJump(-1)<CR>
nnoremap <silent> <buffer> ]] :call MyCppFunctionBeginJump(1)<CR>
