" set font
if has("win32")
    "set guifont=ProggyClean:h8
    "set guifont=ProggyTinySZ:h8
    "set guifont=Dina_TTF:h8
    "set guifont=Consolas:h8
    "set guifont=Lucida_Console:h9
    "set guifont=Inconsolata:h9
    "set guifont=fixed613:h8
    "set guifont=Terminus:h9
    "set guifont=Terminus\ (TTF):h9
    set guifont=creep2:h8:cANSI:qDEFAULT
    set linespace=0
    if has("directx")
        " set rop=type:directx,taamode:0
    endif
elseif has("gui_macvim")
    set noantialias
    set guifont=Dina_TTF:h11
    set linespace=0
else
    set guifont=Tewi\ 11
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
set guicursor=n-v-c:block-Cursor/lCursor,ve:ver35-Cursor,o:hor50-Cursor,i-ci:ver25-Cursor/lCursor,r-cr:hor20-Cursor/lCursor,sm:block-Cursor-blinkwait175-blinkoff150-blinkon175
set guicursor+=a:blinkon0
"
