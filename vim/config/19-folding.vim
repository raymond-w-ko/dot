" Folding
"function! MyFoldText()
    "let line = getline(v:foldstart)
    "let sub = substitute(line, '^"\s\=\|/\*\|\*/\|{{{\d\=', '', 'g') "}}}
    "let remaining = &columns - len(sub)
    "return sub . repeat(' ', remaining)
"endfunction
"function! SetFoldSettings()
    "if exists("g:my_fold_settings_applied")
        "return
    "endif

    "set foldenable
    "set foldmethod=syntax
    "set foldopen=block,hor,mark,percent,quickfix,tag,search
    "set foldlevelstart=9001
    "set foldnestmax=20

    "let g:my_fold_settings_applied=1
"endfunction
"set foldtext=MyFoldText()
"call SetFoldSettings()

function! MySaveOrigFoldMethod()
    if exists("b:original_fdm")
        return
    endif
    let b:original_fdm=&foldmethod
    setlocal foldmethod=manual
endfunction
augroup SaveOriginalFoldMethod
    au!
    au InsertEnter * call MySaveOrigFoldMethod()
augroup END

" remake all 
nnoremap zM a<ESC>:setl foldmethod=<C-R>=b:original_fdm<CR><CR>zM:setl fdm=manual<CR>

nnoremap <Space><Space> za
vnoremap <Space><Space> za

" enable syntax folding for XML (caution, this can be slow)
let g:xml_syntax_folding=1
