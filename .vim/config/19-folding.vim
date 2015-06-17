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

" hopefully replaced by fastfold.vim
"function! MySaveOrigFoldMethod()
    "if exists("b:original_foldmethod")
        "return
    "endif
    "let b:original_foldmethod=&foldmethod
    "setlocal foldmethod=manual
"endfunction
"augroup SaveOriginalFoldMethod
    "au!
    "au InsertEnter * call MySaveOrigFoldMethod()
"augroup END
"nnoremap zM a<ESC>:setl foldmethod=<C-R>=b:original_foldmethod<CR><CR>zM:setl fdm=manual<CR>

" enable syntax folding for XML (caution, this can be slow)
"let g:xml_syntax_folding=1

nmap <Space><Space> za
vmap <Space><Space> za
set foldlevelstart=9001
