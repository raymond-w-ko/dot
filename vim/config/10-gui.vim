if !exists("g:already_set_color_scheme")
    set background=dark
    let g:lucius_contrast="low"
    let g:lucius_contrast_bg="normal"
    let g:lucius_style="dark"
    let g:lucius_use_bold=0
    let g:lucius_use_underline=0
    colorscheme lucius

    let g:already_set_color_scheme=1
endif

" pretty vertical Splits
set fillchars+=vert:\|
