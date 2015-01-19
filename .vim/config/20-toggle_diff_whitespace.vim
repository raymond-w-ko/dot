" Toggle whitespace in diffs

set diffopt-=iwhite
let g:should_diff_whitespace = 1
function! ToggleDiffWhitespace() "
    if g:should_diff_whitespace
        set diffopt-=iwhite
        let g:should_diff_whitespace = 0
    else
        set diffopt+=iwhite
        let g:should_diff_whitespace = 1
    endif
    diffupdate
endfunction

nnoremap <leader>dw :call ToggleDiffWhitespace()<CR>
