let s:uname = "win32"
if has("unix")
    let s:uname = system("uname")
endif
if (s:uname == "Darwin\n")
    if has("+macmeta")
        set macmeta
    endif
endif

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ALL GLORY TO THE ESC KEY
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" this caused too much stress to right hand and was a bit awkward for me
"inoremap jk <Esc>
" I only hit this like 5% of the time
"inoremap kj <Esc>

" trying out something crazy
" too much stress to middle fingers
"inoremap dk <Esc>
"inoremap kd <Esc>
" just right :-)
inoremap fj <Esc>
inoremap jf <Esc>

" mirror dd and D, a bit hard to get use to
nnoremap Y y$

" looking at junegunn's vimrc, <C-f> and <C-b> are usually overkill
noremap <C-F> <C-D>
noremap <C-B> <C-U>

" lazyness, and to help me use Ex commands
nnoremap ; :
nnoremap : ;
vnoremap ; :
vnoremap : ;

"lazy write
if (!exists('g:lazy_writing') || g:lazy_writing == 0)
    function! MyLazyWrite()
        let g:lazy_writing = 1
        silent wall
        "let curdir = expand("%:p:h")
        "let makefile = curdir . '/make.bat'
        "if (filereadable(makefile)) && match(getcwd(), '\v\CSVN.Syandus_') != -1
            "silent !make.bat
        "endif
        let g:lazy_writing = 0
        "SyntasticCheck
    endfunction
endif
nnoremap <silent> <CR> :call MyLazyWrite()<CR>

" some convenience mappings for Vim autocomplete
inoremap <C-l> <C-x><C-l>

" disable crazy keys
nnoremap K <Nop>
vnoremap K <Nop>
nnoremap ZZ <Nop>
nnoremap ZQ <Nop>

" nobody uses EX mode, use Q for formatting instead
nnoremap Q gqip
vnoremap Q gq

nnoremap ' `
nnoremap ` '

" <Ctrl-Backspace>
nnoremap  :BD<CR>
nnoremap <C-BS> :BD<CR>

" Press F12 to switch to UTF-8 encoding
nnoremap <F11> :e ++enc=latin1<CR>
nnoremap <F12> :e ++enc=utf8<CR>

" General {{{
" Substitute
nnoremap <leader>s :%s//
vnoremap <leader>s :s//

nnoremap <leader>\ :s/\//\\/<CR>:nohlsearch<CR>
nnoremap <leader>/ :s/\\/\//<CR>:nohlsearch<CR>

" CTRL-V and are Paste
"inoremap <C-v> <C-r>=@+<CR>

" below obsoleted: by tpope's vim-rsi
" CTRL-hjkl movement while in : command mode
" <C-h> interferes with <BS> key
"cnoremap <C-h> <Left>
"cnoremap <C-l> <Right>
"cnoremap <C-j> <Down>
"cnoremap <C-k> <Up>

" Platform specific keybinds
if has("unix")
    cmap w!! w !sudo tee % >/dev/null

    nnoremap <leader>ev :e ~/.vimrc<CR>
    nnoremap <leader>gc :CtrlP ~/.vim/config<CR>
elseif has("win32")
    if isdirectory('C:/cygwin/home/rko')
        exe 'nnoremap <leader>ev :e C:/cygwin/home/rko/dot/.vimrc<CR>'
        exe 'nnoremap <leader>gc :CtrlP C:/cygwin/home/rko/dot/.vim/config<CR>'
    endif
    if isdirectory('C:/cygwin64/home/rko')
        exe 'nnoremap <leader>ev :e C:/cygwin64/home/rko/dot/.vimrc<CR>'
        exe 'nnoremap <leader>gc :CtrlP C:/cygwin64/home/rko/dot/.vim/config<CR>'
    endif
    if isdirectory('C:/cygwin/home/root')
        exe 'nnoremap <leader>ev :e C:/cygwin/home/root/dot/.vimrc<CR>'
        exe 'nnoremap <leader>gc :CtrlP C:/cygwin/home/root/dot/.vim/config<CR>'
    endif

    nnoremap <leader>eh :e C:/Windows/system32/drivers/etc/hosts<CR>
    "nnoremap <leader>el :e C:/SVN/_my_launch.bat<CR>
endif

function! FindFileInProjectDirectory()
    execute ':CtrlP ' . EscapePathname(MyGetProjectDirectory())
endfunction
nnoremap <leader>t :call FindFileInProjectDirectory()<CR>

nnoremap <leader>b :CtrlPBuffer<CR>

function! MyAlternateFunction()
    let old_buf_nr = bufnr('%')
    A
    let new_buf_nr = bufnr('%')
    if (old_buf_nr != new_buf_nr)
        call CenterCursorAesthetically()
    endif
endfunction
nnoremap <leader>a :call MyAlternateFunction()<CR>
nnoremap <leader>o :ToggleWord<CR>

function! MyPasteToggle()
    if (&paste)
        set nopaste
    else
        set paste
    endif
endfunction
nnoremap <leader>p :call MyPasteToggle()<CR>

" This allows for change paste motion cp{motion}
" http://stackoverflow.com/questions/2471175/vim-replace-word-with-contents-of-paste-buffer
nnoremap <silent> cp :set opfunc=ChangePaste<CR>g@
function! ChangePaste(type, ...)
    silent exe "normal! `[v`]\"_c"
    silent exe "normal! p"
endfunction

" lazy braces
function! MyDoubleBracesExpander()
    let line = strpart(getline('.'), 0, col('.') - 1)
    let line_len = strlen(line)
    if (line_len < 2)
        return
    endif

    if ( line[line_len - 2] != '{' ||
       \ line[line_len - 1] != '{' )
        return
    endif
    call feedkeys("\<BS>\<CR>X\<CR>}\<Up>\<End>\<BS>", 't')
endfunction
augroup ExpandDoubleBraces
    au!
    au CursorMovedI * call MyDoubleBracesExpander()
augroup END

function! CreateCppMethodImplementation()
    " determine the complete function definition
    let line_num = line('.')

    " find the line with '(', this marks the beginning
    while (1)
        let cur_line = getline(line_num)
        if (match(cur_line, '\v\(') != -1)
            break
        endif

        let line_num = line_num - 1
    endwhile

    let begin_line_num = line_num
    let begin_line = getline(begin_line_num)
    let definition_whitespace = PrecedingWhitespaceCount(begin_line)

    " find the line with ')', this marks the end
    while (1)
        let cur_line = getline(line_num)
        if (match(cur_line, '\v\)') != -1)
            break
        endif

        let line_num = line_num + 1
    endwhile

    let end_line_num = line_num

    if (exists('s:RefactorCppFunctionDefinition'))
        unlet s:RefactorCppFunctionDefinition
    endif
    let s:RefactorCppFunctionDefinition = getline(begin_line_num, end_line_num)
    let index = end_line_num - begin_line_num
    let last_line = s:RefactorCppFunctionDefinition[index]
    let last_line = substitute( last_line, '\s*=\s*0;', ';', 'g')
    let s:RefactorCppFunctionDefinition[index] = last_line

    " determine the class name
    " we will just go up until we see a line begin with 'class'
    let line_num = begin_line_num - 1
    while (1)
        let cur_line = getline(line_num)
        let words = split(cur_line, '\W\+')

        if (len(words) >= 2)
            if (words[0] == 'class' || words[0] == 'struct')
                if (PrecedingWhitespaceCount(cur_line) < definition_whitespace)
                    if (exists('g:RefactorCppClassName'))
                        unlet g:RefactorCppClassName
                    endif
                    let g:RefactorCppClassName = words[1]
                    break
                endif
            endif
            if (words[0] == 'namespace')
                let g:RefactorCppClassName = words[1]
                break
            endif
        endif

        let line_num = line_num - 1
    endwhile

    A
    set fo-=r
    set fo-=o
    execute "normal Go\<ESC>G"
    set fo+=r
    set fo+=o
    call append('$', s:RefactorCppFunctionDefinition)
    normal j
    normal VVG<

    if (expand('<cword>') == "static")
        normal dw
    endif

    "check if have virtual keyword, if so delete it since function declarations
    "don't have that, only in the function definition
    if (expand('<cword>') == "virtual")
        normal dw
    endif

    " TODO check if we have a constructor or destructor, which has no return type

    "insert class name
    let cur_line = getline(line('.'))
    let words = split(cur_line, '\W\+')
    "if (words[0] != g:RefactorCppClassName)
    "if (len(words) > 1)
        "normal W
    "endif

    while 1
        let word = expand('<cWORD>')
        if match(word, '(') != -1
            break
        endif
        normal W
    endwhile

    execute "normal! i\<C-r>=g:RefactorCppClassName\<CR>::\<ESC>G$s\<CR>\<ESC>xxxxxxxx"
endfunction

augroup CppRefactor
    au BufReadPre *.cpp,*.h nnoremap <buffer> <leader>rci :call CreateCppMethodImplementation()<CR>dd$a<Space>{{
augroup END


" lazy .. to ->
function! MyLazyDotDotToArrow()
    if (match(&filetype, '\vc$|cpp|objc|php') == -1)
        return
    endif

    let line = strpart(getline('.'), 0, col('.') - 1)
    let line_len = strlen(line)
    if (line_len < 2)
        return
    endif

    if (line[line_len - 3] != '.' &&
      \ line[line_len - 3] != '/' &&
      \ line[line_len - 3] != '\' &&
      \ line[line_len - 2] == '.' &&
      \ line[line_len - 1] == '.')
        call feedkeys("\<BS>\<BS>->", 'n')
    endif

    if (line[line_len - 3] == '-' &&
      \ line[line_len - 2] == '>' &&
      \ line[line_len - 1] == '.')
        call feedkeys("\<BS>\<BS>\<BS>...", 'n')
    endif
endfunction
augroup ConvertTwoDotsToArrow
    au!
    au CursorMovedI * call MyLazyDotDotToArrow()
augroup END

"}}}
" Splits {{{
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

nnoremap <leader>wv :vsplit<CR>
nnoremap <leader>ws :split<CR>
nnoremap <leader>wc :close<CR>
nnoremap <leader>wo :wincmd o<CR>
nnoremap <leader>w} :wincmd }<CR>
nnoremap <leader>wg} :wincmd g}<CR>

nnoremap <C-Up> :resize +1<CR>
nnoremap <C-Down> :resize -1<CR>
nnoremap <C-Left> :vertical resize -1<CR>
nnoremap <C-Right> :vertical resize +1<CR>

function! MarkWindowSwap()
    let g:markedWinNum = winnr()
endfunction

function! DoWindowSwap()
    "Mark destination
    let curNum = winnr()
    let curBuf = bufnr( "%" )
    exe g:markedWinNum . "wincmd w"
    "Switch to source and shuffle dest->source
    let markedBuf = bufnr( "%" )
    "Hide and open so that we aren't prompted and keep history
    exe 'hide buf' curBuf
    "Switch to dest and shuffle source->dest
    exe curNum . "wincmd w"
    "Hide and open so that we aren't prompted and keep history
    exe 'hide buf' markedBuf
endfunction

nnoremap <silent> <leader>wm :call MarkWindowSwap()<CR>
nnoremap <silent> <leader>wp :call DoWindowSwap()<CR>

nnoremap <silent> <Left> :call MarkWindowSwap()<CR><C-w>h:call DoWindowSwap()<CR>
nnoremap <silent> <Right> :call MarkWindowSwap()<CR><C-w>l:call DoWindowSwap()<CR>
" }}}
" Tabs {{{
nnoremap <leader>1 1gt
nnoremap <leader>2 2gt
nnoremap <leader>3 3gt
nnoremap <leader>4 4gt
nnoremap <leader>5 5gt
nnoremap <leader>6 6gt
nnoremap <leader>7 7gt
nnoremap <leader>8 8gt
nnoremap <leader>9 9gt

function! CreateScratch()
    1split

    " create preview window
    set winfixheight
    Scratch
    setlocal nowrap
    silent! exe "chdir " . current_directory

    wincmd k
endfunction
function! CreateAndSetupVsplits()
    let num_vsplits = (&columns / &winwidth) - 1

    let num_cur_tabs = tabpagenr('$')
    if ((num_cur_tabs > 1) && !exists('g:my_current_number_of_tabs'))
        let g:my_current_number_of_tabs = num_cur_tabs
    endif

    if !exists("g:my_current_number_of_tabs")
        let g:my_current_number_of_tabs = 1
    endif

    " get the current directory because we want to replicate this
    " in the new tab
    let current_directory = expand("%:p:h")

    " set up our initial tab if this is our first time
    if g:my_current_number_of_tabs > 1
        tabnew
        silent! exe "chdir " . current_directory
    endif

    "call CreateScratch()

    " create number of vsplits based off of argument passwd
    for ii in range(num_vsplits)
        vsplit
        silent! exe "chdir " . current_directory
    endfor

    " move back to left vsplit
    for ii in range(num_vsplits)
        wincmd h
    endfor

    if (num_vsplits >= 2)
        wincmd l
    endif

    wincmd =

    let g:my_current_number_of_tabs += 1
    return
endfunction
nnoremap <leader>wt :call CreateAndSetupVsplits()<CR>
nnoremap <leader>ww :tabclose<CR>

" }}}
" Finding stuff {{{
function! ExtensionHelper(ext, dir)
    let partial = a:dir . '/**/*.' . a:ext
    let partial = EscapePathname(partial) . ' '
    return partial
endfunction
function! GetRelevantExtensions()
  let directory = MyGetProjectDirectory()

  let extensions = ""
  let extensions .= ExtensionHelper('vim', directory)
  let extensions .= ExtensionHelper('ssf', directory)
  let extensions .= ExtensionHelper('sml', directory)
  let extensions .= ExtensionHelper('h', directory)
  let extensions .= ExtensionHelper('c', directory)
  let extensions .= ExtensionHelper('cpp', directory)
  let extensions .= ExtensionHelper('m', directory)

  return extensions
endfunction

function! FindCursorWordInBuffer()
  let filename = EscapePathname(expand('%:p'))
  execute 'LAck! ' . expand("<cword>") . ' ' . EscapePathname(filename)
  lopen
endfunction

function! FindCursorWordInProject()
  execute ':Ack! ' . expand("<cword>") . ' ' . MyGetProjectDirectory()
endfunction

function! FindThisKeywordInProject(keyword)
  execute ':Ack! ' . a:keyword . ' ' . MyGetProjectDirectory()
endfunction

nnoremap <leader>fwib :call FindCursorWordInBuffer()<CR>
nnoremap <leader>fwip :call FindCursorWordInProject()<CR>
nnoremap <leader>fkip :call FindThisKeywordInProject("")<left><left>
nnoremap <leader>fl :FufLine<CR>
" }}}

" Fancy Tag Completion {{{

function! MyCppCompleteFunc(findstart, base)
    " get current line up to where cursor is located
    let line = strpart(getline('.'), 0, col('.'))
    let words = split(line, '\W\+')

    if a:findstart
        " start after the '(' of course
        return col('.') - 1
    else
        let matches = GetFunctionSignatures3(words[-1])
        return { 'words' : matches, 'refresh' : 'always' }
    endif
endfunction

function! GetFunctionSignatures(keyword)
    let results = taglist("^" . a:keyword . "$")
    let possible_function_signatures = []
    for item in results
      if (has_key(item, 'signature'))
        let signature = item['signature']
        let signature = substitute(signature, '\((\s*\)\|\(\s*)\)', "", "g")
        let arg_list = split(signature, '\s*,')

        let class = '-'
        if (has_key(item, 'class'))
          let class = item['class']
        endif

        let entry = class . '::' . a:keyword . '('
        for arg in arg_list
            let arg = substitute(arg, '^\s*\|\s*$', "", 'g')
            let entry .= "\n  " . arg
        endfor
        let entry .= " )"

        call add(possible_function_signatures, entry)
      endif
    endfor

    return possible_function_signatures
endfunction

function! GetFunctionSignatures2(keyword)
    "let results = taglist("^" . a:keyword . "$")
    let results = omegacomplete#taglist(a:keyword)
    let possible_function_signatures = []
    for item in results
      if (has_key(item, 'signature'))
        let signature = iconv(item['signature'], 'latin1', &encoding)

        let class = '-'
        if (has_key(item, 'class'))
          let class = item['class']
        endif

        let return_type = ''
        if (has_key(item, 'prefix'))
            let return_type = item['prefix']
        endif

        let entry = return_type . ' ' . class . '::' . a:keyword . signature
        if (match(signature, '(\s*)') == -1)
            call add(possible_function_signatures, entry)
        endif
      endif
    endfor

    return possible_function_signatures
endfunction

function! GetFunctionSignatures3(keyword)
    let results = taglist("^" . a:keyword . "$")
    let possible_function_signatures = []
    for item in results
      if (has_key(item, 'signature'))
        let entry = {}

        let signature = item['signature']

        let class = '-'
        if (has_key(item, 'class'))
          let class = item['class']
        endif

        let entry['word'] = signature
        let entry['abbr'] = class
        call add(possible_function_signatures, entry)
      endif
    endfor

    return possible_function_signatures
endfunction

function! MySuperLeftParen()
    if (match(&ft, '\v(cpp)') == -1)
        return ''
    endif

    " get current line up to where cursor is located
    let line = strpart(getline('.'), 0, col('.'))

    if (line[strlen(line) - 1] == ' ')
        return ''
    endif

    let words = split(line, '\W\+')
    if (len(words) < 1)
        return ''
    endif

    let last_word = words[-1]
    let possible_function_signatures = GetFunctionSignatures2(last_word)
    let num_sig = len(possible_function_signatures)

    if (num_sig == 0)
        return ''
    endif

    let output = []

    for item in possible_function_signatures
        call add(output, item)
    endfor

    let new_scratch_window_size = len(possible_function_signatures)
    if (new_scratch_window_size > 5)
        let new_scratch_window_size = 5
    endif

    let cur_win_nr = winnr()
    let scratch_win_nr = bufwinnr('__Scratch__')
    if (scratch_win_nr == -1)
        return ''
    endif

    execute scratch_win_nr . "wincmd w"
    "execute 'resize ' . new_scratch_window_size
    normal ggVGD
    call setline(line('.'), output)
    execute cur_win_nr . "wincmd w"

    return ''
endfunction

function! MySuperRightParen()
    if (match(&ft, '\v(cpp)') == -1)
        return ''
    endif

    let cur_win_nr = winnr()
    let scratch_win_nr = bufwinnr('__Scratch__')
    if (scratch_win_nr == -1)
        return ''
    endif

    execute scratch_win_nr . "wincmd w"
    resize 1
    execute cur_win_nr . "wincmd w"

    return ''
endfunction

" <CR> should not autoaccept what the popup menu has selected
if !exists('g:has_set_my_omegacomplete_tab_binding')
    if g:omegacomplete_version_preference == 1
        inoremap <expr> <Tab> omegacomplete#UseFirstEntryOfPopup()
    elseif g:omegacomplete_version_preference == 2
        inoremap <expr> <Tab> omegacomplete2#UseFirstEntryOfPopup()
    endif
    let g:has_set_my_omegacomplete_tab_binding=1
endif
"inoremap <silent>   (       (<C-r>=MySuperLeftParen()<CR>
"inoremap <silent>   )       )<C-r>=MySuperRightParen()<CR>

function! MyChangeNextArg()
    " always start out with an ESC to get out of insert mode
    let change_command = "\<ESC>"
    " yay for zero indexing
    let current_pos = col('.') - 2
    let line = getline('.')

    let char0 = line[current_pos]
    let char1 = line[current_pos + 1]

    " first case ( arg1, or (arg1,
    if ((char0 ==# '(') && (char1 !=# ','))
        let change_command .= 'l'
    elseif (char1 ==# ')')
        return ""
    elseif (char1 ==# ',')
        let change_command .= 'lll'
    endif

    let change_command .= "vt"

    "determine if we even have a ',' to move to
    let ii = 0
    let found_comma = 0
    for ii in range(current_pos + 2, len(line))
        if (line[ii] ==# ',')
        let found_comma = 1
        endif
    endfor

    if (found_comma)
        let change_command .= ','
    else
        let change_command .= ')'
    endif

    let change_command .= "\<C-G>"
    return change_command
endfunction
"inoremap <expr> <S-A-l> MyChangeNextArg()
" }}}

" Super Insert Mode Completion {{{
function! ChooseWordFromPmenu(index)
    if pumvisible() == 0
        return ""
    endif

    let keys = ""
    for ii in range(1, a:index)
        let keys .= "\<C-N>"
    endfor
    let keys .= "\<C-Y>"
    return keys
endfunction
"inoremap <expr> <A-a> ChooseWordFromPmenu(1)
"inoremap <expr> <A-s> ChooseWordFromPmenu(2)
"inoremap <expr> <A-d> ChooseWordFromPmenu(3)
"inoremap <expr> <A-f> ChooseWordFromPmenu(4)
"inoremap <expr> <A-g> ChooseWordFromPmenu(5)
"inoremap <expr> <A-h> ChooseWordFromPmenu(6)
"inoremap <expr> <A-j> ChooseWordFromPmenu(7)
"inoremap <expr> <A-k> ChooseWordFromPmenu(8)
"inoremap <expr> <A-l> ChooseWordFromPmenu(9)
"inoremap <expr> <A-;> ChooseWordFromPmenu(10)

"inoremap <expr> <A-q> ChooseWordFromPmenu(11)
"inoremap <expr> <A-w> ChooseWordFromPmenu(12)
"inoremap <expr> <A-e> ChooseWordFromPmenu(13)
"inoremap <expr> <A-r> ChooseWordFromPmenu(14)
"inoremap <expr> <A-t> ChooseWordFromPmenu(15)
"inoremap <expr> <A-y> ChooseWordFromPmenu(16)
"inoremap <expr> <A-u> ChooseWordFromPmenu(17)
"inoremap <expr> <A-i> ChooseWordFromPmenu(18)
"inoremap <expr> <A-o> ChooseWordFromPmenu(19)
"inoremap <expr> <A-p> ChooseWordFromPmenu(20)
"}}}

" Handle URL
" Stolen from https://github.com/askedrelic/homedir/blob/master/.vimrc
" OSX only: Open a web-browser with the URL in the current line
function! HandleURI()
    let s:uri = matchstr(getline("."), '[a-z]*:\/\/[^ >,;]*')
    echo s:uri
    if s:uri != ""
        exec "!open \"" . s:uri . "\""
    else
        echo "No URI found in line."
    endif
endfunction
map <leader>u :call HandleURI()<CR>

" Split/Join
"
" Basically this splits the current line into two new ones at the cursor position,
" then joins the second one with whatever comes next.
"
" Example:                      Cursor Here
"                                    |
"                                    V
" foo = ('hello', 'world', 'a', 'b', 'c',
"        'd', 'e')
"
"            becomes
"
" foo = ('hello', 'world', 'a', 'b',
"        'c', 'd', 'e')
"
" Especially useful for adding items in the middle of long lists/tuples in Python
" while maintaining a sane text width.
"nnoremap K h/[^ ]<cr>"zd$jyyP^v$h"zpJk:s/\v +$//<cr>:noh<cr>j^

nnoremap <leader>m :make!<CR>

" vim:fdm=marker:foldlevel=0
