" set font
if has("win32")
    set guifont=fixed613:h8
    "set guifont=Dina_TTF:h8
    "set guifont=Consolas:h8
    "set guifont=Lucida_Console:h9
    "set guifont=Inconsolata:h9
    set linespace=0
    if has("directx")
        "set rop=type:directx
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
