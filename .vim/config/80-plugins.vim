" MatchParen
"let g:loaded_matchparen = 1

" a.vim
let g:alternateNoDefaultAlternate=1

" NetRW
"let g:netrw_silent=1
" apparently enabling this hijacks the mouse completely
" so you can't use it to select stuff (WTF!)
"let g:netrw_mousemaps=0
"let g:netrw_cygwin = 1

" AutoComplPop
"let g:acp_enableAtStartup = 0
"let g:acp_ignorecaseOption = 0
"let g:acp_completeOption = '.,w,b,u,t'
"let g:acp_behaviorKeywordLength = 2
"let g:acp_completeoptPreview = 1
"let g:acp_behaviorKeywordIgnores = ['Sy', 'sy', 'get', 'set', 'Get', 'Set']

" Command-T
"let g:CommandTMaxHeight=16
"let g:CommandTMatchWindowAtTop=1
"let g:CommandTMatchWindowReverse=0

" ack.vim
"let g:ackprg="C:/Perl/site/bin/ack.bat -H --nocolor --nogroup --column"

" LustyJuggler
"let g:LustyJugglerDefaultMappings=0

" yankring
"let g:yankring_min_element_length = 2

" tagbar
"let g:tagbar_width = 40
"let g:tagbar_sort = 0

" CtrlP
let g:ctrlp_map = "<C-\\>"           " set to something that I will never use
let g:ctrlp_match_window_reversed = 0
let g:ctrlp_match_window_bottom = 0
let g:ctrlp_max_height = 32
let g:ctrlp_working_path_mode = 0
let g:ctrlp_switch_buffer = 0
let g:ctrlp_clear_cache_on_exit = 0
let g:ctrlp_max_files = 0
"let g:ctrlp_lazy_update = 350

"function! CtrlPMatch(items, str, limit, mmode, ispath, crfile, regex) abort
  "let items = copy(a:items)
  "if a:ispath
    "call filter(items, 'v:val !=# a:crfile')
  "endif
  "return haystack#filter(items, a:str)
"endfunction
" too slow
"let g:ctrlp_match_func = {'match': function('CtrlPMatch')}

if executable('ag')
  set grepprg=ag\ --nogroup\ --nocolor
  let ignored_exts = map(split(&wildignore, ','), '"--ignore \"" . v:val . "\""')
  let ignore_string = join(ignored_exts, ' ')
  let g:ctrlp_user_command = 'ag %s -i --nocolor --nogroup --hidden -g "" ' . ignore_string
endif

if !has('python')
  echo 'In order to use pymatcher plugin, you need +python compiled vim'
else
  let g:ctrlp_match_func = { 'match': 'pymatcher#PyMatch' }
endif

" indent-guides
"let g:indent_guides_enable_on_vim_startup=0

" vim-powerline
let g:Powerline_symbols='compatible'
let g:Powerline_stl_path_style='short'
let g:Powerline_theme='default'
let g:Powerline_colorscheme='default'

" UltiSnips
let g:UltiSnipsExpandTrigger = "<F1>"
let g:UltiSnipsListSnippets = "<C-F1>"
let g:UltiSnipsJumpForwardTrigger = "<C-j>"
let g:UltiSnipsJumpBackwardTrigger = "<C-k>"

" Gundo
"nnoremap <F5> :GundoToggle<CR>
let g:gundo_right=1

" vim-clojure-static
let g:clojure_maxlines = 1024
let g:clojure_align_multiline_strings = 1
let g:clojure_fuzzy_indent = 1
let g:clojure_fuzzy_indent_patterns = ['^with', '^def', '^let']
let g:clojure_fuzzy_indent_blacklist =
    \ ['-fn$', '\v^with-%(meta|out-str|loading-context)$']

" rainbow_parentheses
let g:rbpt_bold=0
let g:rbpt_max=8
let g:rbpt_colorpairs = [
    \ [ '6',  '#2aa198'],
    \ [ '13', '#6c71c4'],
    \ [ '4',  '#268bd2'],
    \ [ '5',  '#d33682'],
    \ [ '1',  '#dc322f'],
    \ [ '2',  '#859900'],
    \ [ '3',  '#b58900'],
    \ [ '9',  '#cb4b16'],
    \ ]
"augroup EnableRainbowParentheses
  "au!
  "au VimEnter * RainbowParenthesesToggle
  "au Syntax * RainbowParenthesesLoadRound
  "au Syntax * RainbowParenthesesLoadBraces
  " breaks Lua's [[multiline string literal]] and --[[megacomment]]
  "au Syntax * RainbowParenthesesLoadSquare
  " breaks XML hilighting
  "au Syntax * RainbowParenthesesLoadChevrons
"augroup END

" rainbow
"let g:rainbow_active = 1
"let g:rainbow_load_separately = [
    "\ [ '*' , [['(', ')'], ['\[', '\]'], ['{', '}']] ],
    "\ [ '*.tex' , [['(', ')'], ['\[', '\]']] ],
    "\ [ '*.cpp' , [['(', ')'], ['\[', '\]'], ['{', '}']] ],
    "\ [ '*.{html,htm}' , [['(', ')'], ['\[', '\]'], ['{', '}'], ['<\a[^>]*>', '</[^>]*>']] ],
    "\ ]
"augroup RainbowParentheses
  "au!
  "au FileType c,cpp,objc,objcpp,go,rust,javascript,java call rainbow#load()
  "au FileType clojure call rainbow#load(
      "\ [['(', ')'], ['\[', '\]'],
      "\ ['{', '}']], '"[-+*/=><%^&$#@!~|:?\\]"')
"augroup END

" vim-niji
let g:niji_dark_colours = [
    \ ['196', 'red1'],
    \ ['214', 'orange1'],
    \ ['226', 'yellow1'],
    \ ['154', 'greenyellow'],
    \ ['46', 'green1'],
    \ ['48', 'springgreen1'],
    \ ['51', 'cyan1'],
    \ ['62', 'slateblue1'],
    \ ['135', 'purple1']]

"NERDCommenter
let g:NERDCustomDelimiters = {
    \ 'syxml': { 'left': '//', 'right': ''},
    \ 'ogre': { 'left': '//', 'right': ''},
    \ 'glsl': { 'left': '//', 'right': ''}
    \ }

" omegacomplete
"let g:omegacomplete_log_file = "C:\\SVN\\omegacomplete.txt"
let g:omegacomplete_normal_hi_cmds=[
    \ "hi Pmenu guifg=#00ff00 guibg=#002b36 gui=none ctermbg=0 ctermfg=046 cterm=none",
    \ "hi PmenuSel guifg=#002b36 guibg=#00ff00 gui=none ctermbg=046 ctermfg=0 cterm=none",
    \ ]

let g:omegacomplete_corrections_hi_cmds=[
    \ "hi Pmenu guifg=#ffff00 guibg=#002b36 gui=none ctermbg=0 ctermfg=226 cterm=none",
    \ "hi PmenuSel guifg=#002b36 guibg=#ffff00 gui=none ctermbg=226 ctermfg=0 cterm=none",
    \ ]

" syntastic
let g:syntastic_enable_highlighting = 1
let g:syntastic_enable_balloons = 1
let g:syntastic_auto_jump = 3
let g:syntastic_enable_signs = 1
let g:syntastic_mode_map = {
            \ 'mode': 'active',
            \ 'active_filetypes': ['ruby', 'php', 'python'],
            \ 'passive_filetypes': ['java', 'c', 'cpp', 'objc', 'objcpp'] }

" paredit
let g:paredit_leader = ','
let g:paredit_shortmaps = 0

" detectindent
let g:detectindent_max_lines_to_analyse = 1024
let g:detectindent_autodetect = 1
let g:detectindent_preferred_indent = 2
let g:detectindent_preferred_expandtab = 1
let g:detectindent_min_indent = 2
" hope to $DEITY that no one uses > 4 indents
let g:detectindent_max_indent = 4

" airline
let g:airline_left_sep=''
let g:airline_right_sep=''
let g:airline_theme='base16'
let g:airline#extensions#whitespace#enabled = 0
let g:airline#extensions#tabline#enabled = 1

" bufkill.vim
let g:BufKillCreateMappings = 0

" vim-cljfmt
let g:clj_fmt_autosave = 0

augroup Clojure
  au!
  au FileType clojure nnoremap <buffer> <leader>r :Require<CR>
  au FileType clojure nnoremap <buffer> <leader>R :Require!<CR>
  au FileType clojure nnoremap <buffer> == :Cljfmt<CR>
augroup END

" base16 default colorscheme
"
let g:synesthesia_banned_console_colors = []
" depends on console theme
for i in range(0, 16)
  call add(g:synesthesia_banned_console_colors, i)
endfor

" too dark
call add(g:synesthesia_banned_console_colors, 17)
call add(g:synesthesia_banned_console_colors, 18)
call add(g:synesthesia_banned_console_colors, 19)
call add(g:synesthesia_banned_console_colors, 52)
call add(g:synesthesia_banned_console_colors, 53)
call add(g:synesthesia_banned_console_colors, 54)
call add(g:synesthesia_banned_console_colors, 55)
call add(g:synesthesia_banned_console_colors, 58)
call add(g:synesthesia_banned_console_colors, 59)
call add(g:synesthesia_banned_console_colors, 60)
call add(g:synesthesia_banned_console_colors, 61)
call add(g:synesthesia_banned_console_colors, 62)

" white color
for i in range(232, 256)
  call add(g:synesthesia_banned_console_colors, i)
endfor

let g:synesthesia_ignored_filetypes = []
call add(g:synesthesia_ignored_filetypes, 'help')
call add(g:synesthesia_ignored_filetypes, 'text')
call add(g:synesthesia_ignored_filetypes, 'diff')
call add(g:synesthesia_ignored_filetypes, 'gitcommit')
call add(g:synesthesia_ignored_filetypes, 'svn')
