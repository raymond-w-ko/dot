nnoremap <silent> <buffer> [[ ?\v^function\|^local\s*function<CR>:nohls<CR>
nnoremap <silent> <buffer> ]] /\v^function\|^local\s*function<CR>:nohls<CR>

setlocal foldmethod=expr
function! MyLuaFoldExpr()
    let line = getline(v:lnum)
    if line =~ '^--.*'
        return 0
    end
    if line =~ '^function.*'
        return '>1'
    endif

    if line =~ '^end.*'
        return '<1'
    end
    return -1
endfunction
setlocal foldexpr=MyLuaFoldExpr()

function! MyLuaFoldText()
    let line = getline(v:foldstart)
    let sub = substitute(line, '^function ', '', '')
    return sub
endfunction
setlocal foldtext=MyLuaFoldText()
