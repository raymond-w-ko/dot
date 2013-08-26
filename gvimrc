"colorscheme neon2

" set font
if has("win32")
    if !exists("g:already_set_font")
        "set guifont=Dina_TTF:h8
        set guifont=Consolas:h8
        "set guifont=MS_Gothic:h8:cSHIFTJIS
        set linespace=0
        if has("directx")
            "set rop=type:directx
        endif

        let g:already_set_font=1
    endif
elseif has("gui_macvim")
    set noantialias
    set guifont=Dina_TTF:h11
    set linespace=0
endif

" set GUI options
if has('win32')
    " disable everything except synced clipboard
    set guioptions=a

    augroup FullScreenOnStartup
        autocmd!
        autocmd GUIEnter * simalt ~x
    augroup END
elseif has("gui_macvim")
    " disable everything except synced clipboard
    set guioptions=a

    " Full screen means FULL screen
    set fuoptions=maxvert,maxhorz
    set lines=9999
    set columns=9999
end

" Remove cursor blink
set guicursor+=a:blinkon0
