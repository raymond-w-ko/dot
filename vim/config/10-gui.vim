if !exists("g:already_set_color_scheme")
    set background=dark
    let g:lucius_style="dark_dim"
    colorscheme lucius

    let g:already_set_color_scheme=1
endif

if (has("gui_running"))
    " Font
    if has("win32")
        if !exists("g:already_set_font")
            set guifont=Dina_TTF:h8
            set linespace=0

            let g:already_set_font=1
        endif
    elseif has("gui_macvim")
        set noantialias
        set guifont=Dina_TTF:h11
        set linespace=0
    endif

    " GUI Configuration
    set guioptions=a          " disable everything except synced clipboard

    " Maximize in Windows automatically
    if has("win32")
        nnoremap <F11> :call libcallnr("gvimfullscreen.dll", "ToggleFullScreen", 1)<CR>
        function! FullScreenVim()
            if !exists("g:already_fullscreen_vim")
                "call libcallnr("gvimfullscreen.dll", "ToggleFullScreen", 1)
                "call libcallnr("gvimfullscreen.dll", "ToggleFullScreen", 1)
                "call libcallnr("gvimfullscreen.dll", "ToggleFullScreen", 1)
                let g:already_fullscreen_vim=1
            endif
        endfunction
        augroup FullScreenOnStartup
            autocmd!
            autocmd BufEnter * call FullScreenVim()
            autocmd GUIEnter * simalt ~x
        augroup END
    elseif has("gui_macvim")
        " Full screen means FULL screen
        set fuoptions=maxvert,maxhorz
    endif

    " Remove cursor blink
    set guicursor+=a:blinkon0

    " pretty vertical Splits
    set fillchars+=vert:│
    "set fillchars=vert:\|,fold:-
endif
