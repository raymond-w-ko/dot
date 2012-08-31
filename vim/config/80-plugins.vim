" MatchParen
"let g:loaded_matchparen = 1

" a.vim
let g:alternateNoDefaultAlternate=1

" NetRW
let g:netrw_silent=1
" apparently enabling this hijacks the mouse completely
" so you can't use it to select stuff (WTF!)
let g:netrw_mousemaps=0
let g:netrw_cygwin = 1
"let g:netrw_scp_cmd = "\"C:\\Program Files (x86)\\PuTTY\\pscp.exe\" -q -agent"

" AutoComplPop {{{
let g:acp_enableAtStartup = 0
let g:acp_ignorecaseOption = 0
let g:acp_completeOption = '.,w,b,u,t'
let g:acp_behaviorKeywordLength = 2
let g:acp_completeoptPreview = 1
let g:acp_behaviorKeywordIgnores = ['Sy', 'sy', 'get', 'set', 'Get', 'Set']

" Command-T
let g:CommandTMaxHeight=16
let g:CommandTMatchWindowAtTop=1
let g:CommandTMatchWindowReverse=0

" ack.vim
let g:ackprg="C:/Perl/site/bin/ack.bat -H --nocolor --nogroup --column"

" LustyJuggler
let g:LustyJugglerDefaultMappings=0

" yankring
let g:yankring_min_element_length = 2

" tagbar
let g:tagbar_width = 40
let g:tagbar_sort = 0

" CtrlP
let g:ctrlp_map = '<leader>\'           " set to something that I will never use
let g:ctrlp_match_window_reversed = 0
let g:ctrlp_match_window_bottom = 0
let g:ctrlp_max_height = 16
let g:ctrlp_working_path_mode = 0
let g:ctrlp_switch_buffer = 1

" indent-guides
let g:indent_guides_enable_on_vim_startup=0

" vim-powerline
let g:Powerline_symbols='compatible'
let g:Powerline_stl_path_style='short'
let g:Powerline_theme='default'
let g:Powerline_colorscheme='default'

" UltiSnips
let g:UltiSnipsExpandTrigger = "<F13>"
let g:UltiSnipsListSnippets = "<C-F13>"
let g:UltiSnipsJumpForwardTrigger = "<C-j>"
let g:UltiSnipsJumpBackwardTrigger = "<C-k>"

" Gundo
nnoremap <F5> :GundoToggle<CR>
let g:gundo_right=1

" omegacomplete
let g:omegacomplete_normal_hi_cmds=[
    \ "hi Pmenu guifg=#00ff00 guibg=#003300 gui=none ctermbg=022 ctermfg=046 cterm=none",
    \ "hi PmenuSel guifg=#003300 guibg=#00ff00 gui=none ctermbg=046 ctermfg=022 cterm=none",
    \ ]

let g:omegacomplete_corrections_hi_cmds=[
    \ "hi Pmenu guifg=#ffff00 guibg=#333300 gui=none ctermbg=058 ctermfg=011 cterm=none",
    \ "hi PmenuSel guifg=#333300 guibg=#ffff00 gui=none ctermbg=011 ctermfg=058 cterm=none",
    \ ]

" vim:fdm=marker:foldlevel=9001
