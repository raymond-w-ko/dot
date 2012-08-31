" Vim color file
" Maintainer:   Raymond W. Ko <raymond.w.ko@gmail.com>
" Last Change:  2011
" URL:          127.0.0.1

" cool help screens
" :he group-name
" :he highlight-groups
" :he cterm-colors

set background=light
hi clear
if exists("syntax_on")
    syntax reset
endif
let g:colors_name="kokonut"

hi Normal         guifg=Black       guibg=#EEEEEE       gui=none
hi Normal         guifg=White     guibg=Black       gui=none
"hi CursorLine     guifg=Black                           gui=none
"hi Visual         guifg=Black       guibg=#CEFF84       gui=none
hi Visual         guifg=Black       guibg=#E1F5C4       gui=none
"hi Visual         guifg=Black       guibg=#99EE00       gui=none
hi MatchParen     guifg=Black       guibg=Gray          gui=bold

hi LineNr         guifg=DarkGray    guibg=#E3E3E3       gui=none

"hi Number         guifg=Black       guibg=#FFEBBF       gui=none
"hi Number         guifg=Black       guibg=#E0FFE0       gui=none

"hi String         guifg=Black       guibg=#F8F8DA       gui=none
"hi Character      guifg=Black       guibg=#F8F8DA       gui=none

hi Comment        guifg=DarkGreen                       gui=none
hi Constant       guifg=#A000A0                         gui=none              
hi Identifier     guifg=#800000                         gui=none
hi Statement      guifg=#0000FF                            gui=none
hi Keyword        guifg=#0000FF                            gui=none
hi PreProc        guifg=#A000A0                         gui=none
hi Type           guifg=#0000FF                            gui=none
hi Underlined     guifg=Black                           gui=underline  

hi Special        guifg=#CC5500                         gui=none
hi Delimiter      guifg=Black                           gui=none

hi Ignore         guifg=Grey
hi Error          guifg=#EEEEEE     guibg=Red           gui=bold
hi Todo           guifg=Black       guibg=Yellow        gui=bold

hi Boolean        guifg=#0000FF                            gui=none
hi Function       guifg=#800000                         gui=none

" Completion Menu
hi Pmenu              guifg=Black       guibg=#FFFFFF
hi PmenuSel           guifg=Black       guibg=#CEFF84
"hi PmenuSbar          guifg=Red         guibg=Red
"hi PmenuThumb         guifg=Green       guibg=Green

" language specific additions
"hi xmlTag         gui=bold    guifg=#0000FF
"hi xmlTagName     gui=bold    guifg=#0000FF

" Misc
hi Cursor         guifg=Black      guibg=#EEEEEE       gui=none
hi CursorIM       guifg=Black       guibg=#EEEEEE       gui=none
hi VertSplit      guifg=Black         guibg=#EEEEEE       gui=none

hi StatusLine     guifg=#EEEEEE       guibg=#3A6EA5     gui=none
hi StatusLineNC   guifg=DarkGray      guibg=#DDDDDD     gui=none

hi IncSearch      guifg=NONE         guibg=Yellow       gui=none
hi Search         guifg=NONE         guibg=Green

"hi Directory
"hi DiffAdd
"hi DiffChange
"hi DiffDelete
"hi DiffText
"hi ErrorMsg
"hi Folded
"hi FoldColumn
"hi ModeMsg
"hi MoreMsg
"hi NonText
"hi Question
"hi SpecialKey
"hi Title
"hi VisualNOS
"hi WarningMsg
"hi WildMenu
"hi Menu
"hi Scrollbar
"hi Tooltip
