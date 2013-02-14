colorscheme neon2

if has("win32")
    if !exists("g:already_set_font")
        "set guifont=Dina_TTF:h8
        "set guifont=Consolas:h10
        set guifont=MS_Gothic:h8:cSHIFTJIS
        set linespace=-1
        if has("directx")
            "set directx
        endif

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
    augroup FullScreenOnStartup
        autocmd!
        autocmd GUIEnter * simalt ~x
    augroup END
elseif has("gui_macvim")
    " Full screen means FULL screen
    set fuoptions=maxvert,maxhorz
endif

" Remove cursor blink
set guicursor+=a:blinkon0

if has('mac')
    set lines=9999
    set columns=9999
endif
