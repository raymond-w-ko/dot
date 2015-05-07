if !exists("g:already_set_color_scheme") && !($TERM == "linux")
    "set background=dark
    "let g:lucius_contrast="low"
    "let g:lucius_contrast_bg="normal"
    "let g:lucius_style="dark"
    "let g:lucius_use_bold=0
    "let g:lucius_use_underline=0
    "colorscheme lucius

    set background=dark
    " seoul256 (dark):
    "   Range:   233 (darkest) ~ 239 (lightest)
    "   Default: 237
    let g:seoul256_background=235
    "colorscheme seoul256
    "set background=light
    "colorscheme seoul256-light

    "let g:solarized_italic = 0
    "let g:solarized_visibility="low"
    "colorscheme solarized

    let base16colorspace=256
    colorscheme base16-3024

    let g:already_set_color_scheme=1
endif

" pretty vertical Splits
set fillchars+=vert:\|

if has("gui_running")
    set guioptions-=m  "remove menu bar
    set guioptions-=T  "remove toolbar
    set guioptions-=r  "remove right-hand scroll bar
    set guioptions-=L  "remove left-hand scroll bar
    set guioptions-=e  "use in editor tabline
    set guifont=Tewi\ 11
    set lines=9999
endif
