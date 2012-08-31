" Folding
function! MyFoldText()
    let line = getline(v:foldstart)
    let sub = substitute(line, '^"\s\=\|/\*\|\*/\|{{{\d\=', '', 'g') "}}}
    let remaining = &columns - len(sub)
    return sub . repeat(' ', remaining)
endfunction
function! SetFoldSettings()
    if exists("g:my_fold_settings_applied")
        return
    endif

    set foldenable
    set foldmethod=syntax
    set foldopen=block,hor,mark,percent,quickfix,tag,search
    set foldlevelstart=9001
    set foldnestmax=20

    let g:my_fold_settings_applied=1
endfunction
set foldtext=MyFoldText()
call SetFoldSettings()

" Don't screw up folds when inserting text that might affect them, until
" leaving insert mode. Foldmethod is local to the window. Protect against
" screwing up folding when switching between windows.
function! MySaveOrigFoldMethod()
    if exists("b:orig_fdm")
        return
    endif

    let b:orig_fdm=&foldmethod
    setlocal foldmethod=manual
endfunction
augroup SaveOriginalFoldMethod
    au!
    au InsertEnter * call MySaveOrigFoldMethod()
augroup END
"autocmd InsertLeave,WinLeave * if exists('w:last_fdm') | let &l:foldmethod=w:last_fdm | unlet w:last_fdm | endif

" Space to toggle folds.

nnoremap zM a<ESC>:setl foldmethod=<C-R>=b:orig_fdm<CR><CR>zM:setl fdm=manual<CR>
function! MyFoldToggle()
    if !exists("b:my_fold_toggle")
        let b:my_fold_toggle = 0
    endif
    if (b:my_fold_toggle == 0)
        normal! zn
        normal zz
        let b:my_fold_toggle = 1
    else
        normal zM
        normal! zz
        let b:my_fold_toggle = 0
    endif
endfunction

"nmap <Space> :call MyFoldToggle()<CR>
nnoremap <Space> za
"vnoremap <Space> za

" Make zO recursively open whatever top level fold we're in, no matter where the
" cursor happens to be.
"nnoremap zO zCzO

" Use <leader>z to "focus" the current fold.
"nnoremap <leader>z zMzvzz

" enable syntax folding for XML (caution, this can be slow)
let g:xml_syntax_folding=1
