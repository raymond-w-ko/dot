if !exists("g:already_set_color_scheme")
    set background=dark
    let g:lucius_contrast="low"
    let g:lucius_contrast_bg="high"
    let g:lucius_style="dark"
    let g:lucius_use_bold=1
    let g:lucius_use_underline=0
    colorscheme lucius

    let g:already_set_color_scheme=1
endif

" pretty vertical Splits
set fillchars+=vert:│

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
endif
