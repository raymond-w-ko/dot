" vim:fdm=marker

" http://utf8everywhere.org/
set encoding=utf-8

augroup MyVimrc
  au!
augroup END

if filereadable(expand("$HOME/.has-full-github-access"))
  let g:plug_url_format = 'git@github.com:%s.git'
  let g:plug_shallow = 0
endif

set runtimepath+=$HOME/.vim/bundle/lightline-colorschemes

let s:cywgin_vim_dir = expand("C:/cygwin64/home/$USERNAME/dot/.vim/bundle")
if isdirectory(s:cywgin_vim_dir)
  call plug#begin(s:cywgin_vim_dir)
else
  call plug#begin('~/.vim/bundle')
endif

" my plugins
Plug 'raymond-w-ko/vim-solarized8'
Plug 'raymond-w-ko/scrollfix'
Plug 'raymond-w-ko/vim-eslisp'
Plug 'raymond-w-ko/vim-lua-indent'
if has("python") || has("python3")
  Plug 'raymond-w-ko/omegacomplete.vim'
endif

" colorscheme
Plug 'lifepillar/vim-colortemplate'

" finders
Plug 'ctrlpvim/ctrlp.vim'
Plug 'FelikZ/ctrlp-py-matcher'
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'

" potpourri
Plug 'vim-jp/vital.vim'
Plug 'kana/vim-operator-user'
Plug 'qpkorr/vim-bufkill'
Plug 'christoomey/vim-tmux-navigator'
Plug 'Konfekt/FastFold'
Plug 'itchyny/lightline.vim'
Plug 'mhinz/vim-startify'
if has('python3') || has('python')
  Plug 'SirVer/ultisnips'
endif
Plug 'honza/vim-snippets'
" Plug 'vim-syntastic/syntastic'
Plug 'w0rp/ale'
Plug 'maximbaz/lightline-ale'
Plug 'sjl/gundo.vim'
Plug 'majutsushi/tagbar'
Plug 'justinmk/vim-dirvish'
Plug 'zhimsel/vim-stay'
Plug 'godlygeek/tabular'
Plug 'mattn/emmet-vim'
Plug 'luochen1990/rainbow'
Plug 'tmux-plugins/vim-tmux-focus-events'

" Tim Pope
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-obsession'
Plug 'tpope/vim-commentary'
" Plug 'tpope/vim-vinegar'
Plug 'tpope/vim-characterize'
Plug 'tpope/vim-rsi'
Plug 'tpope/vim-tbone'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rhubarb'
" E712: Argument of get() must be a List or Dictionary
" Plug 'tpope/vim-ragtag'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-speeddating'
Plug 'tpope/vim-capslock'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-scriptease'
Plug 'tpope/vim-markdown'
Plug 'tpope/vim-git'
Plug 'tpope/vim-haystack'
Plug 'tpope/vim-apathy'

" Hayabusa + friends
Plug 'easymotion/vim-easymotion'
Plug 'haya14busa/incsearch.vim'
Plug 'haya14busa/incsearch-easymotion.vim'
Plug 'haya14busa/vim-asterisk'
" Plug 'haya14busa/is.vim'
Plug 'osyo-manga/vim-anzu'

" Junegunn Choi
Plug 'junegunn/vim-easy-align'
Plug 'junegunn/vim-peekaboo'
Plug 'junegunn/gv.vim'

" LucHermitte
Plug 'LucHermitte/lh-vim-lib'
Plug 'LucHermitte/alternate-lite'

" Web Development
Plug '2072/PHP-Indenting-for-VIm'
Plug 'pangloss/vim-javascript'
" this package has now been deprecated
" Plug 'mxw/vim-jsx'
Plug 'hail2u/vim-css3-syntax'
Plug 'othree/csscomplete.vim'
Plug 'groenewege/vim-less'

" universal REPL
Plug 'jpalardy/vim-slime'

" reason lang
Plug 'reasonml-editor/vim-reason-plus'

" clojure
  " the way of fireplace
" Plug 'raymond-w-ko/vim-fireplace', {'branch': 'debug'}
" if !has('win32unix') " this plugin makes any file access extremely slow...
"   Plug 'tpope/vim-salve'
" endif
" Plug 'tpope/vim-classpath'
" Plug 'guns/slamhound'
" Plug 'venantius/vim-cljfmt'
  " plasmaplace
Plug 'raymond-w-ko/vim-plasmaplace'
  " universal
Plug 'guns/vim-clojure-static'
Plug 'guns/vim-sexp'
Plug 'tpope/vim-sexp-mappings-for-regular-people'

" misc filetypes
Plug 'rhysd/vim-clang-format'
Plug 'octol/vim-cpp-enhanced-highlight'
Plug 'aklt/plantuml-syntax'

call plug#end()

let g:omegacomplete_version_preference=1
" let g:omegacomplete_quick_select_keys="asdfjklgh"
if has('java')
  let jar_list = split(globpath(expand('$HOME') . '/java', '*.jar'), "\n")
  call insert(jar_list, expand('$VIMRUNTIME') . '/vim.jar', 0)
  let jars = substitute(join(jar_list, ';'), '\\', '/', 'g')
  let jars = substitute(jars, ' ', '\\ ', 'g')
  exe "set javacp=" . jars
  javarepl clojure
endif

" General {{{

" load sensible defaults by our prophet Tim Pope
runtime! plugin/sensible.vim

if has('win32')
  let s:data_dir="$APPDATA/Vim"
  set viewdir=$APPDATA/Vim/view
elseif match(system('uname'), "Darwin") > -1
  let s:data_dir='~/Library/Vim'
elseif empty($XDG_DATA_HOME)
  let s:data_dir='~/.local/share/vim'
else
  let s:data_dir='$XDG_DATA_HOME/vim'
endif
if isdirectory(expand(s:data_dir))
  if &directory =~# '^\.,'
    let &directory = expand(s:data_dir) . '/swap//,' . &directory
  endif
  if &backupdir =~# '^\.,'
    let &backupdir = expand(s:data_dir) . '/backup//,' . &backupdir
  endif
  if exists('+undodir') && &undodir =~# '^\.\%(,\|$\)'
    let &undodir = expand(s:data_dir) . '/undo//,' . &undodir
  endif
endif
if exists('+undofile')
  set undofile
endif
set noswapfile  " computers are pretty reliable nowadays

" Leader
let mapleader = "\<Space>"
let maplocalleader = ","

if !exists("g:rko_already_turned_syntax_off")
  syntax off
  let g:rko_already_turned_syntax_off=1
endif
set fileformats=unix,dos
set autowrite
set autowriteall
set updatetime=500
set shortmess+=aIc    " no intro message, no ins-completion-menu
set report=0 " report back when greater than N lines changed
set showmode
set hidden
set novisualbell
set noerrorbells
if exists('+belloff')
  set belloff=all
endif
set nonumber
set ruler
if exists('+relativenumber')
  set norelativenumber
endif
" setting this to 10000 actually causes noticable exit lag
set history=128
" lazy redraw breaks vim-anzu
" set lazyredraw
set showcmd
set ttyfast
set matchtime=0
set splitbelow
set splitright
set title
set showtabline=2
set cmdheight=3
set completeopt+=menu
set completeopt+=menuone
set completeopt+=preview
set pumheight=16
" this breaks dirvish
" set autochdir
set nolist
" always try to make the current window 80 columns
set winwidth=80
set nojoinspaces
set maxmempattern=2000000
set maxmem=2000000
set maxmemtot=2000000
set listchars=tab:\|\ ,trail:-,extends:>,precedes:<,nbsp:+
" Mouse & selection Behavior
behave xterm                " of course xterm is better
set selectmode=""           " never want SELECT mode
set mousemodel=popup
set keymodel=""
set selection=inclusive
set mousehide
set nomousefocus
set mouse=a
if has("mouse_sgr")
  set ttymouse=sgr
else
  set ttymouse=xterm2
end
set clipboard=autoselect
set pastetoggle=<F9>

" this breaks vim-capslock since you can't press <C-g>c in less than 100ms
" augroup MyVimrc
"   autocmd InsertEnter * set timeoutlen=100
"   autocmd InsertLeave * set timeoutlen=750
" augroup END

" timeout is needed due to completing fj with omegacomplete
set timeout
set timeoutlen=4096
set ttimeout
set ttimeoutlen=256    " needed to avoid leaving insert mode delay for vim-airline

" no curdir and no sesdir == file names are stored with absolute paths
set sessionoptions-=folds
set sessionoptions-=curdir
set sessionoptions-=sesdir
set sessionoptions-=localoptions
set sessionoptions-=resize
set sessionoptions-=winresize
set sessionoptions-=winsize

set viewoptions=cursor,folds,slash,unix

set cinoptions=
set cinoptions+=:0
set cinoptions+=g0
set cinoptions+=N-s
" this by inteself breaks vim-synesthesia + vim-niji parent parsing
exe "set cinoptions+=(0"
set cinoptions+=u0
set cinoptions+=Ws
set cinoptions+=l1
set cinoptions+=j1
set cinoptions+=J1

set expandtab
set tabstop=2
set shiftwidth=2
set softtabstop=2
" 3 shiftwidths is a little excessive
let g:vim_indent_cont=4

set textwidth=0           " no automatic text wrapping
set formatoptions=qn1
  set fo=
  set fo+=t   " auto-wrap text using textwidth
  set fo+=c   " auto-wrap comments using textwidth, insert comment leader
  set fo+=q   " allow formatting comments with 'gq'
  set fo+=l   " long lines are not broken in insert mode
  if v:version > 702 || (v:version == 702 && has('patch541'))
      set fo+=j   " remove comment leader when joining lines.
  endif
set wrap
set wrapscan
if exists("+breakindent")
    set breakindent
    set breakindentopt=min:20,shift:2,sbr
    set showbreak=>>
else
    exe 'set showbreak=>>\ '
endif

" wildmenu completion
set wildmode=list:longest,full
set wildchar=<Tab>
if exists('&wildignorecase')
  set wildignorecase
endif

" disable bold in terminal
set t_md=

" Ps = 0  -> blinking block.
" Ps = 1  -> blinking block (default).
" Ps = 2  -> steady block.
" Ps = 3  -> blinking underline.
" Ps = 4  -> steady underline.
" Ps = 5  -> blinking bar (xterm).
" Ps = 6  -> steady bar (xterm).
" cursor options
let &t_SI = "\e[6 q"
let &t_EI = "\e[4 q"
" reset cursor on start:
augroup MyVimrc
  au VimEnter * silent !echo -ne "\e[4 q"
augroup END

" binaries with a 99.9% chance of not being edited
set wildignore+=*.exe,*.dll
" media files in a binary format
set wildignore+=*.jpg,*.jpeg,*.png,*.gif,*.bmp,*.tga,*.mp3,*.ico,*.wav
set wildignore+=*.bik,*.ani,*.mask,*.dds
set wildignore+=*.pvr,*.ktx
" version control directories
" adding .git breaks vim-fugitive
"set wildignore+=.hg,.git,.svn
" Visual Studio files
set wildignore+=*.ncb,*.suo,*.user,*.vcproj,*.vcxproj,*.out,*.sln,*.pdb
set wildignore+=*.manifest,*.dep,*.idb,*.ipch,*.o,*.obj
" GCC
set wildignore+=*.d,*.a,*.o
" Gamebryo Binaries
set wildignore+=*.nif,*.kf,*.kfm,*.nsb
" compiled cached bytecodes
set wildignore+=*.pyc,*.luac,*.luc,*.class
" binary document formats
set wildignore+=*.pdf,*.doc,*.docx,*.xls,*.xlsx
" Mac OS X metadata files
set wildignore+=.DS_Store
" Windows OS metadata files
set wildignore+=*.lnk
" Syandus Files
set wildignore+=*.ID,*.sse,*.ccv,*.fls,*.pat,*.gsl,*.flt,*.asi
" OGRE
set wildignore+=*.mesh
" Android Files
set wildignore+=*.apk,*.ap_

" }}}
" Function Library {{{
" Since I keep my projects in the UNIX-ish HOME directory, we have to figure
" out where it is. The problem is that it is potentially different everywhere.
if has('win32')
  let s:unix_home = expand('$HOME')
  let dir = 'C:/cygwin/home/root'
  if isdirectory(dir) | let s:unix_home = dir | endif
  let dir = 'C:/cygwin64/home/root'

  if isdirectory(dir) | let s:unix_home = dir | endif
  let dir = 'C:/cygwin/home/Raymond W. Ko'
  if isdirectory(dir) | let s:unix_home = dir | endif
  let dir = 'C:/cygwin64/home/Raymond W. Ko'
  if isdirectory(dir) | let s:unix_home = dir | endif

  let dir = 'C:/cygwin/home/rko'
  if isdirectory(dir) | let s:unix_home = dir | endif
  let dir = 'C:/cygwin64/home/rko'
  if isdirectory(dir) | let s:unix_home = dir | endif
elseif has('win32unix')
  let s:unix_home = expand('$HOME')
else
  let s:unix_home = expand('$HOME')
endif

" Figure out where the company SVN directory lives
if has('win32')
  let s:svn_home = 'C:'
elseif has('win32unix')
  let s:svn_home = '/cygdrive/c'
else
  let s:svn_home = expand('$HOME')
endif

function! MyTranslateDirectory(dir)
  let dir = a:dir
  let dir = substitute(dir, '__SVN__', s:svn_home, '')
  let dir = substitute(dir, '__UNIX_HOME__', s:unix_home, '')
  let dir = substitute(dir, ' ', '\ ', '')

  if has('win32')
    let dir = substitute(dir, '/', '\\', 'g')
  endif

  return dir
endfunction

let s:commands = [
    \ 'Omegacomplete',      '__UNIX_HOME__/dot/.vim/bundle/omegacomplete.vim',
    \ 'Omegacomplete2',     '__UNIX_HOME__/dot/.vim/bundle/omegacomplete2',
    \ 'OcularWM',           '__UNIX_HOME__/src/ocularwm',
    \ 'Windmenu',           '__UNIX_HOME__/src/windmenu',
    \ 'Dk2test',            '__UNIX_HOME__/src/dk2test',
    \ 'Collimator',         '__UNIX_HOME__/src/collimator',
    \ 'Diffractor',         '__UNIX_HOME__/src/diffractor',
    \ 'LetterDungeon',      '__UNIX_HOME__/src/letterdungeon',
    \
    \ 'Sydocs',             '__UNIX_HOME__/src/sydocs',
    \
    \ 'Engine',             '__UNIX_HOME__/src/alivesim/engine',
    \
    \ 'Diabetes',           '__UNIX_HOME__/src/alivesim/apps/Diabetes_CMESim_2015',
    \ 'Gibleed',            '__UNIX_HOME__/src/alivesim/apps/gi_bleed_2016',
    \ ]

let s:project_directories_list = []
let g:my_project_directories = {}

for i in range(len(s:commands) / 2)
    let cmd = (i * 2) + 0

    let dir = s:commands[(i * 2) + 1]
    let dir = MyTranslateDirectory(dir)
    let g:my_project_directories[dir] = 1

    exe 'command! ' . s:commands[cmd] . ' silent cd ' . dir
endfor

" traverse up parent directories until it finds one that matches in the above
" list
function! s:IsProjectDirectory(directory)
  if isdirectory(a:directory . "/.git")
    return 1
  elseif isdirectory(a:directory . "/.hg")
    return 1
  elseif filereadable(a:directory . "/shadow-cljs.edn")
    return 1
  elseif has_key(g:my_project_directories, a:directory)
    return 1
  else
    return 0
  endif
endfunction
function! MyGetProjectDirectory()
  let last_directory = ''
  let directory = getcwd()

  while !s:IsProjectDirectory(directory) && last_directory != directory
    let last_directory = directory
    let directory = substitute(simplify(directory . '/..'),
        \ '[\\/]*$', '', '')
  endwhile

  if last_directory == directory
    return getcwd()
  elseif has('win32')
    return directory . '\'
  else
    return directory . '/'
endfunction

" command to delete all empty buffers in case you have over 9000 of them
function! DeleteEmptyBuffers()
  let empty = []
  let [i, nbuf] = [1, bufnr('$')]
  while i <= nbuf
      if bufexists(i) && bufname(i) == ''
          let empty += [i]
      endif
      let i += 1
  endwhile
  if len(empty) > 0
      execute 'bdelete ' . join(empty, ' ')
  endif
endfunction
command! DeleteEmptyBuffers call DeleteEmptyBuffers()

" escape pathname with spaces so it doesn't break other commands and functions
function! EscapePathname(pathname)
  return substitute(a:pathname, "\\ ", "\\\\ ", "g")
endfunction

" ex command for toggling hex mode - define mapping if desired
command! -bar HexMode call ToggleHex()

" helper function to toggle hex mode
function! ToggleHex()
  " hex mode should be considered a read-only operation
  " save values for modified and read-only for restoration later,
  " and clear the read-only flag for now
  let l:modified=&mod
  let l:oldreadonly=&readonly
  let &readonly=0
  let l:oldmodifiable=&modifiable
  let &modifiable=1
  if !exists("b:editHex") || !b:editHex
    " save old options
    let b:oldft=&ft
    let b:oldbin=&bin
    " set new options
    setlocal binary " make sure it overrides any textwidth, etc.
    silent :e " this will reload the file without trickeries
              "(DOS line endings will be shown entirely )
    let &ft="xxd"
    " set status
    let b:editHex=1
    " switch to hex editor
    %!xxd
  else
    " restore old options
    let &ft=b:oldft
    if !b:oldbin
      setlocal nobinary
    endif
    " set status
    let b:editHex=0
    " return to normal editing
    %!xxd -r
  endif
  " restore values for modified and read only state
  let &mod=l:modified
  let &readonly=l:oldreadonly
  let &modifiable=l:oldmodifiable
endfunction

function! PrecedingWhitespaceCount(line)
  let num_space = 0
  for i in range(0, strlen(a:line))
    if (match(a:line[i], '\v\W') != -1)
      let num_space = num_space + 1
    else
      break
    endif
  endfor

  return num_space
endfunction

function! FilterSmartQuotes()
    %s/\v“|”/\'/
endfunction
command! FilterSmartQuotes silent! call FilterSmartQuotes()

function! FixSmartQuotes()
    %s/\v/‘/
    %s/\v/’/
    %s/\v/“/
    %s/\v/”/
endfunction
command! FixSmartQuotes silent! call FixSmartQuotes()

command! WriteUTF8 write ++enc=utf-8

function! StripTrailingWhitespace()
    let l:my_saved_winview = winsaveview()
    silent! %s/\s\+$//
    call winrestview(l:my_saved_winview)
endfunction
command! StripTrailingWhitespace call StripTrailingWhitespace()

" use aesthetic middle of screen for "zz"
function! CenterCursorAesthetically()
  normal! zz

  let center = round(winheight(0) / 2.0)
  let offset = winheight(0) * 0.1
  let final = center - offset
  let rounded_final = float2nr(final)
  let rounded_offset = float2nr(offset)
  let delta = winline() - (rounded_final + 1)

  if delta > 0
    exe 'normal ' . delta . "\<C-e>"
  endif
endfunction

function! PropagatePasteBufferToRemote(line1, line2)
  call system('cat_to_remote_clipboard.sh', getreg("*"))
  echo "propagated * register to remote clipboard"
endfunction
command! -range=% PropagatePasteBufferToRemote
    \ call PropagatePasteBufferToRemote(<line1>, <line2>)

if filereadable('/dev/clipboard')
  function! SetClipboard(type, ...) range
    let sel_save = &selection
    let &selection = "inclusive"
    let reg_save = @@
    if a:type == 'n'
      silent exe a:firstline . "," . a:lastline . "y"
    elseif a:type == 'c'
      silent exe a:1 . "," . a:2 . "y"
    else
      silent exe "normal! `<" . a:type . "`>y"
    endif
    "call system('putclip', @@)
    "As of Cygwin 1.7.13, the /dev/clipboard device was added to provide
    "access to the native Windows clipboard. It provides the added benefit
    "of supporting utf-8 characters which putclip currently does not. Based
    "on a tip from John Beckett, use the following:
    call writefile(split(@@,"\n"), '/dev/clipboard', 'b')
    let &selection = sel_save
    let @@ = reg_save
  endfunction

  function! GetClipboard()
    let reg_save = @@
    "let @@ = system('getclip')
    "Much like Putclip(), using the /dev/clipboard device to access to the
    "native Windows clipboard for Cygwin 1.7.13 and above. It provides the
    "added benefit of supporting utf-8 characters which getclip currently does
    "not. Based again on a tip from John Beckett, use the following:
    let @@ = join(readfile('/dev/clipboard'), "\n")
    setlocal paste
    exe 'normal p'
    setlocal nopaste
    let @@ = reg_save
  endfunction
endif

" }}}
" GUI {{{
if (has("termguicolors"))
  set termguicolors
endif
" let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
" let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
if !exists("g:already_set_color_scheme") && !($TERM == "linux")
    set background=dark

    " let base16colorspace=256
    " colorscheme preto

    let g:solarized_italics = 0
    colorscheme solarized8_flat

    let g:already_set_color_scheme=1
endif
if !has("gui_running")
  " need this otherwise colors disappear
  if !exists('g:has_set_my_console_vim_settings')
    set t_Co=256
    let g:has_set_my_console_vim_settings = 1
  endif
endif

" pretty vertical Splits
set fillchars+=stl:\ ,stlnc:\ ,vert:\|

if has("gui_running")
    set guioptions-=m  "remove menu bar
    set guioptions-=T  "remove toolbar
    set guioptions-=r  "remove right-hand scroll bar
    set guioptions-=L  "remove left-hand scroll bar
    set guioptions-=e  "use in editor tabline
    if !has('win32')
      set lines=9999
    endif
endif
" }}}
" Searching and Movement {{{
set ignorecase
set smartcase
set hlsearch
set gdefault            " inverts the meaning of the g-flag in s///g

set virtualedit+=block
set sidescroll=1
set sidescrolloff=1

nnoremap <silent> <leader>l :nohlsearch<CR>:let @/=''<CR>:call clearmatches()<CR>

function! s:get_visual_selection()
  " Why is this not a built-in Vim script function?!
  let [line_start, column_start] = getpos("'<")[1:2]
  let [line_end, column_end] = getpos("'>")[1:2]
  let lines = getline(line_start, line_end)
  if len(lines) == 0
    return ''
  endif
  let lines[-1] = lines[-1][: column_end - (&selection == 'inclusive' ? 1 : 2)]
  let lines[0] = lines[0][column_start - 1:]
  return join(lines, "\n")
endfunction
function! s:selection()
  try
    let a_save = @a
    normal! gv"ay
    return @a
  finally
    let @a = a_save
  endtry
endfunction
function! s:rg_handler(file)
  let i = stridx(a:file, ":")
  if i > 0
    exe "edit " . a:file[0:i - 1]
  endif
endfunction
function! s:FindWordInProject()
  let needle = s:get_visual_selection()
  let dir = MyGetProjectDirectory()
  call fzf#run({
      \ "source": "rg -- " . needle,
      \ "sink": function("s:rg_handler"),
      \ "options": printf('--color="dark,hl:33,hl+:#ff0000,fg+:235,bg+:#000000,fg+:254,info:254,prompt:37,spinner:108,pointer:235,marker:235" --prompt "%s"', dir),
      \ "dir": dir,
      \ "window": {"width": 0.618, "height": 0.618,},})
endfunction
vnoremap <leader>r :call <SID>FindWordInProject()<CR>

" Visual Mode */# from Scrooloose {{{
" function! s:VisualModeSetSearch()
"     let temp = @@
"     norm! gvy
"     let @/ = '\V' . substitute(escape(@@, '\'), '\n', '\\n', 'g')
"     let @@ = temp
" endfunction

" vnoremap * :<C-u>call <SID>VisualModeSetSearch()<CR>//<CR><c-o>
" vnoremap # :<C-u>call <SID>VisualModeSetSearch()<CR>??<CR><c-o>

" Easier to type, and I never use the default behavior.
nnoremap H ^
nnoremap L g_

" Open a Quickfix window for the last search.
nnoremap <silent> <leader>/ :execute 'lvimgrep /'.@/.'/g %'<CR>:lopen<CR>

" Fix linewise visual selection of various text objects
nnoremap VV V
nnoremap Vit vitVkoj
nnoremap Vat vatV
nnoremap Vab vabV
nnoremap VaB vaBV

" It's 2011.
" nnoremap <silent> j gj
" nnoremap <silent> k gk
" this variant by Petr Zemek allows j and k to work properly whe nusing
" relative line numbers
noremap <silent> <expr> j (v:count == 0 ? 'gj' : 'j')
noremap <silent> <expr> k (v:count == 0 ? 'gk' : 'k')

" treat leading whitespace as though it was not there
function! MyLeftBrace()
    let line_number = line('.')
    let starting_line_number = line_number
    let line_number -= 1

    while line_number >= 1
        if match(getline(line_number), '^\s*$') != -1
            break
        endif
        let line_number -= 1
    endwhile

    if line_number != starting_line_number && line_number != 0
        exe 'normal! ' . line_number . 'G'
    elseif line_number == 0
        normal! 1G
    else
        return
    endif

    normal! 0

    return
endfunction
exe "nnoremap <silent> { :call MyLeftBrace()<CR>"

function! MyRightBrace()
    let line_number = line('.')
    let starting_line_number = line_number
    let line_number += 1

    let max_bounds = line('$')

    while line_number <= max_bounds
        if (match(getline(line_number), '^\s*$') != -1)
            break
        endif
        let line_number += 1
    endwhile

    if line_number != starting_line_number && line_number <= max_bounds
        exe 'normal! ' . line_number . 'G'
    elseif line_number > max_bounds
        normal! G
    else
        return
    endif

    normal! 0

    return
endfunction
exe "nnoremap <silent> } :call MyRightBrace()<CR>"

" Highlight word {{{
nnoremap <silent> <leader>hh :execute 'match InterestingWord1 /\<<c-r><c-w>\>/'<cr>
nnoremap <silent> <leader>h1 :execute 'match InterestingWord1 /\<<c-r><c-w>\>/'<cr>
nnoremap <silent> <leader>h2 :execute '2match InterestingWord2 /\<<c-r><c-w>\>/'<cr>
nnoremap <silent> <leader>h3 :execute '3match InterestingWord3 /\<<c-r><c-w>\>/'<cr>
" LOL, these aren't defined
nnoremap <silent> <leader>h4 :execute '4match InterestingWord4 /\<<c-r><c-w>\>/'<cr>
nnoremap <silent> <leader>h5 :execute '5match InterestingWord5 /\<<c-r><c-w>\>/'<cr>
nnoremap <silent> <leader>h6 :execute '6match InterestingWord6 /\<<c-r><c-w>\>/'<cr>
" }}}
" }}}
" }}}
" Syntax and Folding {{{
syntax sync fromstart
"function! MyFoldText()
    "let line = getline(v:foldstart)
    "let sub = substitute(line, '^"\s\=\|/\*\|\*/\|{{{\d\=', '', 'g') "}}}
    "let remaining = &columns - len(sub)
    "return sub . repeat(' ', remaining)
"endfunction
"function! SetFoldSettings()
    "if exists("g:my_fold_settings_applied")
        "return
    "endif

    "set foldenable
    "set foldmethod=syntax
    "set foldopen=block,hor,mark,percent,quickfix,tag,search
    "set foldlevelstart=9001
    "set foldnestmax=20

    "let g:my_fold_settings_applied=1
"endfunction
"set foldtext=MyFoldText()
"call SetFoldSettings()

" enable syntax folding for XML (caution, this can be slow)
"let g:xml_syntax_folding=1

" nmap <leader><leader> za
" vmap <leader><leader> za
set foldlevelstart=9001
" }}}
" Toggle Diff Whitespace {{{
set diffopt-=iwhite
let g:should_diff_whitespace = 1
function! ToggleDiffWhitespace() "
    if g:should_diff_whitespace
        set diffopt-=iwhite
        let g:should_diff_whitespace = 0
    else
        set diffopt+=iwhite
        let g:should_diff_whitespace = 1
    endif
    diffupdate
endfunction

nnoremap <leader>dw :call ToggleDiffWhitespace()<CR>
" }}}
" Text Objects {{{
" Shortcut for []
onoremap ir i[
onoremap ar a[
vnoremap ir i[
vnoremap ar a[

" Shortcut for ()
onoremap if i(
onoremap af a(
vnoremap if i(
vnoremap af a(

" Shortcut for {}
onoremap ic i{
onoremap ac a{
vnoremap ic i{
vnoremap ac a{

" Shortcut for ""
onoremap iq i"
onoremap aq a"
vnoremap iq i"
vnoremap ac a"

" Next and Last

" Motion for "next/last object". For example, "din(" would go to the next "()" pair
" and delete its contents.

onoremap an :<c-u>call <SID>NextTextObject('a', 'f')<cr>
xnoremap an :<c-u>call <SID>NextTextObject('a', 'f')<cr>
onoremap in :<c-u>call <SID>NextTextObject('i', 'f')<cr>
xnoremap in :<c-u>call <SID>NextTextObject('i', 'f')<cr>

onoremap al :<c-u>call <SID>NextTextObject('a', 'F')<cr>
xnoremap al :<c-u>call <SID>NextTextObject('a', 'F')<cr>
onoremap il :<c-u>call <SID>NextTextObject('i', 'F')<cr>
xnoremap il :<c-u>call <SID>NextTextObject('i', 'F')<cr>

function! s:NextTextObject(motion, dir)
  let c = nr2char(getchar())

  if c ==# "b"
      let c = "("
  elseif c ==# "B"
      let c = "{"
  elseif c ==# "d"
      let c = "["
  endif

  exe "normal! ".a:dir.c."v".a:motion.c
endfunction
" }}}
" Mappings {{{
let s:uname = "win32"
if has("unix")
    let s:uname = system("uname")
endif
if (s:uname == "Darwin\n")
    if has("+macmeta")
        set macmeta
    endif
endif

"               ,'``.._   ,'``.
"              :,--._:)\,:,._,.:       All Glory to
"              :`--,''   :`...';\      the ESC KEY!
"               `,'       `---'  `.
"               /                 :
"              /                   \
"            ,'                     :\.___,-.
"           `...,---'``````-..._    |:       \
"             (                 )   ;:    )   \  _,-.
"              `.              (   //          `'    \
"               :               `.//  )      )     , ;
"             ,-|`.            _,'/       )    ) ,' ,'
"            (  :`.`-..____..=:.-':     .     _,' ,'
"             `,'\ ``--....-)='    `._,  \  ,') _ '``._
"          _.-/ _ `.       (_)      /     )' ; / \ \`-.'
"         `--(   `-:`.     `' ___..'  _,-'   |/   `.)
"             `-. `.`.``-----``--,  .'
"               |/`.\`'        ,','); SSt
"                   `         (/  (/

inoremap kj <Esc>

" normalize Y to act like D and C
map Y y$

" fastest way to save a file
nnoremap <silent> <leader>w :wall<CR>
nnoremap <CR> :w<CR>

" some convenience mappings for Vim autocomplete
inoremap <C-l> <C-x><C-l>

" nobody uses EX mode, use Q for formatting instead
nnoremap Q gqip
vnoremap Q gq

nnoremap ' `
nnoremap ` '

" Substitute
nnoremap <leader>s :%s//
vnoremap <leader>s :s//

nnoremap <leader>\ :s/\//\\/<CR>:nohlsearch<CR>
nnoremap <leader>/ :s/\\/\//<CR>:nohlsearch<CR>

" killing buffers without closing current split
nnoremap <DEL> :BD<CR>

" tab navigation
nnoremap <Home> :tabprev<CR>
nnoremap <End> :tabnext<CR>
nnoremap [1~ :tabprev<CR>
nnoremap [4~ :tabnext<CR>

let s:list_of_pairs = [
    \ ['(', ')'],
    \ ['[', ']'],
    \ ['{', '}'],
    \ ['"', '"'],
    \ ["'", "'"],
    \ ]
function! s:EmptyPairDeleterBackspace()
  let line = getline('.')
  let n = strlen(line)
  let pos = col('.')
  if pos <= 1 || pos > n
    return "\<BS>"
  endif

  let left = line[pos-2]
  let right = line[pos-1]

  for pairs in s:list_of_pairs
    if left == pairs[0] && right == pairs[1]
      return "\<C-g>U\<Right>\<BS>\<BS>"
    endif
  endfor

  return "\<BS>"
endfunction

function! s:MySmarterCR()
  let keys = ""
  if pumvisible()
    let keys .= "\<C-e>"
  endif

  let line = getline('.')
  let n = strlen(line)
  let pos = col('.')
  if pos <= 1 || pos > n
    return keys . "\<CR>"
  endif

  let left = line[pos-2]
  let right = line[pos-1]

  for pairs in s:list_of_pairs
    if left == pairs[0] && right == pairs[1]
      return keys . "\<CR>\<Esc>O"
    endif
  endfor
  return keys . "\<CR>"
endfunction
inoremap <Plug>MySmarterCR <C-r>=<SID>MySmarterCR()<CR>

function! s:MyBasicCR()
  let keys = ""
  if pumvisible()
    let keys .= "\<C-e>"
  endif

  return keys . "\<CR>"
endfunction
inoremap <Plug>MyBasicCR <C-r>=<SID>MyBasicCR()<CR>

function! s:SetupPairBindings()
  " handled by vim-sexp
  if &ft == 'clojure' || &ft == 'lisp' || &ft == 'scheme'
    exe "imap <silent><buffer> φ ("
    exe "imap <silent><buffer> σ {"
    exe "imap <silent><buffer> ρ ["
    exe 'imap <silent><buffer> θ "'
    inoremap <silent><buffer> <CR> <C-r>=<SID>MyBasicCR()<CR>
  else
    " semimap helpers
    inoremap <silent><buffer> φ ()<C-g>U<Left>
    inoremap <silent><buffer> σ {}<C-g>U<Left>
    inoremap <silent><buffer> ρ []<C-g>U<Left>
    inoremap <silent><buffer> θ ""<C-g>U<Left>
    inoremap <silent><buffer> υ <><C-g>U<Left>
    inoremap <silent><buffer> <CR> <C-r>=<SID>MySmarterCR()<CR>
"
    if &filetype != "clojure"
      inoremap <silent><buffer> ( ()<C-g>U<Left>
      inoremap <silent><buffer> { {}<C-g>U<Left>
      inoremap <silent><buffer> [ []<C-g>U<Left>
      inoremap <silent><buffer> " ""<C-g>U<Left>
      inoremap <silent><buffer> <CR> <C-r>=<SID>MySmarterCR()<CR>
      inoremap <silent><buffer> <BS> <C-r>=<SID>EmptyPairDeleterBackspace()<CR>
    endif
  endif
endfunction

augroup MyVimrc
  au FileType * call <SID>SetupPairBindings()
augroup END

let s:move_right_keystroke = "\<C-g>U\<Right>"
let s:move_right_pair_ends = { "'" : 1, '"' : 1, ')' : 1, ']' : 1, '}' : 1 }
function! s:MyPareditForwardUp()
  let keys = ''
  if pumvisible()
    let keys .= "\<C-y>"
  endif

  let line = getline('.')
  let n = strlen(line)
  let steps_right = 0
  let x = col('.') - 1
  if n > 0
    while x < n
      let ch = line[x]
      if has_key(s:move_right_pair_ends, ch)
        break
      endif

      let steps_right += 1
      let x += 1
    endwhile
  endif

  if n == 0 || x == n
    " do nothing
  else
    let keys .= repeat(s:move_right_keystroke, steps_right+1)
  endif

  return keys
endfunction
inoremap <Plug>MyPareditForwardUp <C-r>=<SID>MyPareditForwardUp()<CR>

inoremap <expr> χ <SID>MyPareditForwardUp()
inoremap <expr> <Right> <SID>MyPareditForwardUp()

" Platform specific keybinds
if has("unix")
    cmap w!! w !sudo tee % >/dev/null

    nnoremap <leader>ev :e ~/dot/.vimrc<CR>
elseif has("win32")
    if isdirectory('C:/cygwin/home/rko')
        exe 'nnoremap <leader>ev :e C:/cygwin/home/rko/dot/.vimrc<CR>'
    endif
    if isdirectory('C:/cygwin64/home/rko')
        exe 'nnoremap <leader>ev :e C:/cygwin64/home/rko/dot/.vimrc<CR>'
    endif
    if isdirectory('C:/cygwin/home/root')
        exe 'nnoremap <leader>ev :e C:/cygwin/home/root/dot/.vimrc<CR>'
    endif
endif

if executable("fzf")
  function! MyFindFileInProjectAndEdit(sink)
      " execute ':Files ' . EscapePathname(MyGetProjectDirectory())
      let dir = MyGetProjectDirectory()
      call fzf#run({
          \ "sink": a:sink,
          \ "options": printf('--prompt "%s"', dir),
          \ "dir": dir,
          \ "window": {"width": 0.618, "height": 0.618,},})
  endfunction

  nnoremap <leader>b :Buffers<CR>
  nnoremap <C-p> :call MyFindFileInProjectAndEdit('edit')<CR>
  nnoremap <leader>et :call MyFindFileInProjectAndEdit('tabedit')<CR>
else
  " use ctrlp.vim
  function! MyFindFileInProjectAndEdit()
      execute ':CtrlP ' . EscapePathname(MyGetProjectDirectory())
  endfunction

  nnoremap <leader>b :CtrlPBuffer<CR>
  nnoremap <C-p> :call MyFindFileInProjectAndEdit()<CR>
endif

function! MyAlternateFunction()
    " let old_buf_nr = bufnr('%')
    A
    " let new_buf_nr = bufnr('%')
    " if (old_buf_nr != new_buf_nr)
    "     call CenterCursorAesthetically()
    " endif
endfunction
nnoremap <leader>a :call MyAlternateFunction()<CR>
nnoremap <leader><leader> <C-^>
nnoremap <leader>o :ToggleWord<CR>

" This allows for change paste motion cp{motion}
" http://stackoverflow.com/questions/2471175/vim-replace-word-with-contents-of-paste-buffer
function! ChangePaste(type, ...)
    silent exe "normal! `[v`]\"_c"
    silent exe "normal! p"
endfunction
nnoremap <silent> cp :set opfunc=ChangePaste<CR>g@

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

augroup MyVimrc
  au FileType cpp exe "nmap <buffer> <leader>rci :call CreateCppMethodImplementation()<CR>dd$a<Space>σ<CR>"
augroup END

" obsoleted by vim-tmux-navigator
" nmap <C-h> <C-w>h
" nmap <C-j> <C-w>j
" nmap <C-k> <C-w>k
" nmap <C-l> <C-w>l

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
" nnoremap <silent> <leader>wm :call MarkWindowSwap()<CR>
" nnoremap <silent> <leader>wp :call DoWindowSwap()<CR>
nnoremap <silent> <S-Left> :call MarkWindowSwap()<CR><C-w>h:call DoWindowSwap()<CR>
nnoremap <silent> <S-Right> :call MarkWindowSwap()<CR><C-w>l:call DoWindowSwap()<CR>

nnoremap <leader>tq 1gt
nnoremap <leader>tw 2gt
nnoremap <leader>te 3gt
nnoremap <leader>tr 4gt
nnoremap <F1> :tabnew<CR>

function! CreateAndSetupVsplits()
  let num_tabs=tabpagenr("$")
  if num_tabs == 1
    if winnr("$") > 1
      tabnew
    endif
  else
      tabnew
  endif
  
  let num_vsplits = (&columns / (80 - 1)) - 1

  " create number of vsplits based off of argument passwd
  for i in range(num_vsplits)
    vnew
  endfor

  " move back to left vsplit
  for i in range(num_vsplits)
    wincmd h
  endfor

  wincmd =
endfunction
nnoremap <leader>tt :call CreateAndSetupVsplits()<CR>

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
        inoremap <silent><expr> <Tab> omegacomplete#use_first_entry_of_popup()
    elseif g:omegacomplete_version_preference == 2
        inoremap <silent><expr> <Tab> omegacomplete2#use_first_entry_of_popup()
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

" }}}
" autocommands {{{
" set cursorline    " needed as netrw uses the global value to save and restore state
"set cursorcolumn  " needed as netrw uses the global value to save and restore state

" ----------------------------------------------------------------------------
" Help in new tabs
" ----------------------------------------------------------------------------
function! s:SetupHelpTab()
  if &buftype == 'help'
    silent wincmd T
    nnoremap <buffer> q :q<cr>
  endif
endfunction

function! s:SetupParenthesesHightlight()
  " highlight SubtleParentheses ctermfg=240 guifg=#585858
  " syntax match SubtleParentheses /)}]/ containedin=ALL
  " syntax match SubtleParentheses /)}]/ contained
endfunction

let s:double_quote_string_filestypes = {
    \ "javascript.jsx": 1,
    \ "javascript": 1,
    \ "clojure": 1,
    \ "make": 1,
    \ "c": 1,
    \ "cpp": 1,
    \ "python": 1,
    \ "css": 1,
    \ "scss": 1,
    \ "html": 1,
    \ }
let s:single_quote_string_filestypes = {
    \ "javascript.jsx": 1,
    \ "javascript": 1,
    \ "make": 1,
    \ "c": 1,
    \ "cpp": 1,
    \ "python": 1,
    \ "css": 1,
    \ "scss": 1,
    \ }
let s:no_escape_double_quote_string_filestypes = {
    \ "make": 1,
    \ }
let s:double_slash_comment_filestypes = {
    \ "javascript.jsx": 1,
    \ "javascript": 1,
    \ "c": 1,
    \ "cpp": 1,
    \ "css": 1,
    \ "scss": 1,
    \ }
let s:python_style_comment_filestypes = {
    \ "python": 1,
    \ "gitcommit": 1,
    \ "sh": 1,
    \ "make": 1,
    \ "yaml": 1,
    \ }
let s:lisp_style_comment_filestypes = {
    \ "clojure": 1,
    \ }
let s:c_comment_filestypes = {
    \ "javascript.jsx": 1,
    \ "javascript": 1,
    \ "c": 1,
    \ "cpp": 1,
    \ "css": 1,
    \ "scss": 1,
    \ }
let s:c_preprocessor_comment_filestypes = {
    \ "c": 1,
    \ "cpp": 1,
    \ }
let s:version_control_filetypes = {
    \ "gitcommit": 1,
    \ }
function! s:SetupBasicSyntaxHighlights()
  syntax clear
  silent! syntax clear rkoBasicString
  silent! syntax clear rkoBasicComment
  silent! syntax clear rkoMultiLineString
  silent! syntax clear rkoVersionControlDelete
  silent! syntax clear rkoVersionControlAdd
  silent! syntax clear gitMergeConflict

  if &filetype == "vim"
    syntax region rkoBasicString start=/\v"/ skip=/\v(\\\\)|(\\")/ end=/\v("|$)/ keepend
    syntax region rkoBasicString start=/\v'/ skip=/\v(\\')/ end=/\v('|$)/ keepend
  endif

  if has_key(s:double_quote_string_filestypes, &filetype)
    syntax region rkoBasicString start=/\v"/ skip=/\v(\\\\)|(\\")/ end=/\v"/
  endif
  if has_key(s:single_quote_string_filestypes, &filetype)
    syntax region rkoBasicString start=/\v'/ skip=/\v(\\\\)|(\\')/ end=/\v'/
  endif

  if has_key(s:no_escape_double_quote_string_filestypes, &filetype)
    syntax region rkoBasicString start=/\v"/ end=/\v"/
  endif
  if &filetype == "python"
    syn region rkoMultiLineString
        \ start=+[uU]\=\z('''\|"""\)+ end="\z1" keepend
  endif
  if has_key(s:double_slash_comment_filestypes, &filetype)
    syntax region rkoBasicComment start=/\v\/\// end=/\v$/
  endif
  if has_key(s:python_style_comment_filestypes, &filetype)
    syntax region rkoBasicComment start=/\v#/ end=/\v$/
  endif
  if has_key(s:lisp_style_comment_filestypes, &filetype)
    syntax region rkoBasicComment start=/\v;+/ end=/\v$/
  endif
  if has_key(s:c_comment_filestypes, &filetype)
    syntax region rkoBasicComment start=/\v\/\*/ end=/\v\*\//
  endif
  if has_key(s:c_preprocessor_comment_filestypes, &filetype)
    syntax region rkoCPreprocessorComment start=/\v^\s*#if\s+0/ end=/#endif$/
    syntax region rkoCPreprocessorIf start=/\v^\s*#\s*if\s+[a-zA-Z]+/ end=/$/
    syntax region rkoCPreprocessorIfDef start=/\v^\s*#\s*ifdef/ end=/$/
    syntax region rkoCPreprocessorIfNdef start=/\v^\s*#\s*ifndef/ end=/$/
    syntax region rkoCPreprocessorElse start=/\v^\s*#\s*else/ end=/$/
    syntax region rkoCPreprocessorElIf start=/\v^\s*#\s*elif/ end=/$/
    syntax region rkoCPreprocessorEndif start=/\v^\s*#\s*endif/ end=/$/
    syntax region rkoCPreprocessorDefine start=/\v^\s*#\s*define/ end=/$/
  endif
  highlight link rkoBasicString String
  highlight link rkoMultiLineString String
  highlight link rkoBasicComment Comment
  highlight link rkoCPreprocessorComment Comment
  highlight link rkoCPreprocessorIf PreProc
  highlight link rkoCPreprocessorIfDef PreProc
  highlight link rkoCPreprocessorIfNdef PreProc
  highlight link rkoCPreprocessorElse PreProc
  highlight link rkoCPreprocessorElIf PreProc
  highlight link rkoCPreprocessorEndif PreProc
  highlight link rkoCPreprocessorDefine PreProc

  if has_key(s:version_control_filetypes, &filetype)
    syntax region rkoVersionControlDelete start=/\v^-/ end=/\v$/
    syntax region rkoVersionControlAdd start=/\v^\+/ end=/\v$/
  endif
  highlight link rkoVersionControlDelete DiffDelete
  highlight link rkoVersionControlAdd DiffAdd

  highlight link gitMergeConflict Error
  syntax match gitMergeConflict /^=======$/ containedin=ALL
  syntax match gitMergeConflict /^<<<<<<< .\+$/ containedin=ALL
  syntax match gitMergeConflict /^>>>>>>> .\+$/ containedin=ALL

  if &filetype == "clojure"
    runtime plugin/rko_clojure.vim
  elseif &filetype == "dirvish"
    runtime syntax/dirvish.vim
  endif
endfunction

augroup MyVimrc
  " only show cursorline if a window has focus
  " this noticably slows down VIM in files with complicated syntax highlighting,
  " like PHP, so disable it for now.
  au VimEnter,WinEnter,BufWinEnter * setlocal cursorline
  au WinLeave * setlocal nocursorline

  "au VimEnter,WinEnter,BufWinEnter * setlocal cursorcolumn
  "au WinLeave * setlocal nocursorcolumn

  " check when cursor stops moving
  au CursorHold,CursorHoldI * :silent! checktime
  " hack for console VIM so that check for changed files work correctly
  au FocusGained,BufEnter * :silent! checktime

  " hardcore autochdir
  autocmd BufEnter * silent! lcd %:p:h
  autocmd BufEnter * silent! cd %:p:h

  " save all buffers when losing focus
  "au FocusLost * silent! wall

  " Make sure Vim returns to the same line when you reopen a file.
  " Thanks, Amit
  au BufReadPost *
      \ if line("'\"") > 0 && line("'\"") <= line("$") |
      \     execute 'normal! g`"zv' |
      "\     call CenterCursorAesthetically() |
      \ endif
  " this however is annoying for git commit messages
  au BufReadPost COMMIT_EDITMSG exe 'normal! gg'

  " generates too many annoying deltas in open source projects like OGRE
  "au BufWritePre C:/SVN/* call StripTrailingWhitespace()
  "au BufWritePre *.h,*.hpp,*.c,*.cc,*.cpp,*.java,*.py,*.lua call StripTrailingWhitespace()

  " help in new tab to avoid interfering with existing tab layout
  autocmd BufEnter *.txt call s:SetupHelpTab()

  " de-emphasized parentheses
  autocmd Syntax * call s:SetupParenthesesHightlight()
  autocmd FileType * call s:SetupBasicSyntaxHighlights()
  autocmd BufEnter * :syntax sync fromstart
augroup END

" }}}
" Plugins {{{

let g:loaded_matchparen = 1

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" a.vim
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:alternateNoDefaultAlternate=1

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" NetRW
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! <SID>EditFileDirectory()
  let p = expand("%:p:h")
  let cmd = "edit " . p
  execute cmd
endfunction
" this breaks sometimes by consuming window splits
" nnoremap - :call <SID>EditFileDirectory()<CR>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" CtrlP
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:ctrlp_map = "F12"           " set to something that I will never use
let g:ctrlp_max_height = 32
let g:ctrlp_working_path_mode = 0
let g:ctrlp_switch_buffer = 0
let g:ctrlp_clear_cache_on_exit = 1
let g:ctrlp_max_files = 0
let g:ctrlp_lazy_update = 0

"function! CtrlPMatch(items, str, limit, mmode, ispath, crfile, regex) abort
  "let items = copy(a:items)
  "if a:ispath
    "call filter(items, 'v:val !=# a:crfile')
  "endif
  "return haystack#filter(items, a:str)
"endfunction
" too slow
"let g:ctrlp_match_func = {'match': function('CtrlPMatch')}

let g:ctrlp_custom_ignore = {
  \ 'dir': '\v[\/]\.(git|hg|svn)$|[\/]compiled[\/]out$|[\/]node_modules$|[\/]__pycache__$',
  \ }

if executable("rg")
  set grepprg=rg\ --color=never
  let g:ctrlp_user_command = 'rg %s --files --color=never --glob ""'
  let g:ctrlp_use_caching = 0
elseif executable('ag')
  set grepprg=ag\ --nogroup\ --nocolor
  let g:ctrlp_use_caching = 0
  let exts=&wildignore
  let exts .= ',.git,.hg,.svn,node_modules,__pycache__'
  let ignored_exts = map(split(exts, ','), '"--ignore \"" . v:val . "\""')
  let ignore_string = join(ignored_exts, ' ')
  let g:ctrlp_user_command = 'ag %s -i --nocolor --nogroup --hidden -g "" ' . ignore_string
endif

if !has("python") && !has("python3")
  echom 'In order to use pymatcher plugin, you need +python or +python3 compiled vim'
else
  let g:ctrlp_match_func = { 'match': 'pymatcher#PyMatch' }
endif

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" fzf.vim
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" lightline
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:lightline = {
    \ 'colorscheme': 'rko',
    \ 'mode_map': { 'c': 'NORMAL' },
    \ 'active': {
    \   'left': [
    \     ['mode', 'paste'],
    \     ["linter_errors", "linter_warnings", "linter_ok"],
    \     ['fugitive','filename']]
    \ },
    \ 'component_function': {
    \   'modified': 'LightLineModified',
    \   'readonly': 'LightLineReadonly',
    \   'fugitive': 'LightLineFugitive',
    \   'filename': 'LightLineFilename',
    \   'fileformat': 'LightLineFileformat',
    \   'filetype': 'LightLineFiletype',
    \   'fileencoding': 'LightLineFileencoding',
    \   'mode': 'LightLineMode',
    \ },
    \ }
    " \ 'separator': { 'left': '⮀', 'right': '⮂' },
    " \ 'subseparator': { 'left': '⮁', 'right': '⮃' }

let g:lightline.component_expand = {
    \  'linter_warnings': 'lightline#ale#warnings',
    \  'linter_errors': 'lightline#ale#errors',
    \  'linter_ok': 'lightline#ale#ok',
    \ }
let g:lightline.component_type = {
    \     'linter_warnings': 'warning',
    \     'linter_errors': 'error',
    \ }

function! LightLineModified()
  return &ft =~ 'help\|vimfiler\|gundo' ? '' : &modified ? '+' : &modifiable ? '' : '-'
endfunction

function! LightLineReadonly()
  return &ft !~? 'help\|vimfiler\|gundo' && &readonly ? '[RO]' : ''
  " return &ft !~? 'help\|vimfiler\|gundo' && &readonly ? '⭤' : ''
endfunction

function! MyGetShortenedPath()
  let path = expand('%:p')
  let idx = strlen(path)
  for i in range(3)
    let idx = strridx(path, '/', idx) - 1
    if idx <= 0
      break
    endif
  endfor
  if idx <= - 1
    let idx = 0
  else
    let idx += 1
  endif
  let path = path[idx:]
  return path
endfunction

function! LightLineFilename()
  return ('' != LightLineReadonly() ? LightLineReadonly() . ' ' : '') .
      \ (&ft == 'vimfiler' ? vimfiler#get_status_string() :
      \  &ft == 'unite' ? unite#get_status_string() :
      \  &ft == 'vimshell' ? vimshell#get_status_string() :
      \ '' != expand('%:t') ? MyGetShortenedPath() : '[No Name]') .
      \ ('' != LightLineModified() ? ' ' . LightLineModified() : '')
endfunction

function! LightLineFugitive()
  if &ft !~? 'vimfiler\|gundo' && exists("*fugitive#head")
    let _ = fugitive#head()
    return strlen(_) ? '(branch) '._ : ''
    " return strlen(_) ? '⭠ '._ : ''
  endif
  return ''
endfunction

function! LightLineFileformat()
  return winwidth(0) > 70 ? &fileformat : ''
endfunction

function! LightLineFiletype()
  return winwidth(0) > 70 ? (strlen(&filetype) ? &filetype : 'no ft') : ''
endfunction

function! LightLineFileencoding()
  return winwidth(0) > 70 ? (strlen(&fenc) ? &fenc : &enc) : ''
endfunction

function! LightLineMode()
  return winwidth(0) > 60 ? lightline#mode() : ''
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" UltiSnips
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:UltiSnipsExpandTrigger = "<F5>"
let g:UltiSnipsListSnippets = "<F1>"
let g:UltiSnipsJumpForwardTrigger = "<C-j>"
let g:UltiSnipsJumpBackwardTrigger = "<C-k>"

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Gundo
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"nnoremap <F5> :GundoToggle<CR>
let g:gundo_right=1

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" vim-clojure-static
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" By default, parenthesized compound forms that look like function calls and
" whose head subform is on its own line have subsequent subforms indented by
" two spaces relative to the opening paren:
" (foo
"   bar
"   baz)
" Setting this option changes this behavior so that all subforms are aligned to
" the same column, emulating the default behavior of clojure-mode.el:
" (foo
"  bar
"  baz)
" ---> 1 is recommended by the Clojure style guide
" https://github.com/bbatsov/clojure-style-guide#one-space-indent
" ;; good
" (filter
"  even?
"  (range 1 10))
"
" (or
"  ala
"  bala
"  portokala)
"
" ;; bad - two-space indent
" (filter
"   even?
"   (range 1 10))
"
" (or
"   ala
"   bala
"   portokala)
let g:clojure_align_subforms = 1
" default is 100
let g:clojure_maxlines = 512

" this seems to suggest that it should be off
" https://github.com/bbatsov/clojure-style-guide#align-docstring-lines
let g:clojure_align_multiline_strings = 0

let g:clojure_fuzzy_indent = 1
let g:clojure_fuzzy_indent_patterns = ['^with', '^def', '^let', '^go-loop$', '^comment$']
let g:clojure_fuzzy_indent_blacklist = ['-fn$', '\v^with-%(meta|out-str|loading-context)$']
let g:clojure_special_indent_words = 'deftype,defrecord,reify,proxy,extend-type,extend-protocol,letfn,comment'

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" vim-sexp
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:sexp_filetypes = 'clojure,scheme,lisp,timl,eslisp'
" Toggle this for vim-sexp to not go into insert mode after wrapping something
let g:sexp_insert_after_wrap = 1
" Toggle this to disable automatically creating closing brackets and quotes
let g:sexp_enable_insert_mode_mappings = 1

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" vim-niji
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
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

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" omegacomplete
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:omegacomplete_normal_hi_cmds=[
    \ "hi Pmenu guifg=#00ff00 guibg=#002b36 gui=none ctermbg=0 ctermfg=46 term=none cterm=none",
    \ "hi PmenuSel guifg=#002b36 guibg=#00ff00 gui=none ctermbg=46 ctermfg=0 term=none cterm=none",
    \ ]

let g:omegacomplete_corrections_hi_cmds=[
    \ "hi Pmenu guifg=#ffff00 guibg=#002b36 gui=none ctermbg=0 ctermfg=226 term=none cterm=none",
    \ "hi PmenuSel guifg=#002b36 guibg=#ffff00 gui=none ctermbg=226 ctermfg=0 term=none cterm=none",
    \ ]
inoremap <expr> <C-s> omegacomplete#toggle_pause_completion()

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" syntastic
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:syntastic_enable_highlighting = 1
let g:syntastic_enable_balloons = 1
let g:syntastic_auto_jump = 0
let g:syntastic_enable_signs = 1
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_mode_map = {
            \ "mode": "active",
            \ "active_filetypes": ["ruby", "php", "python", "yaml"],
            \ "passive_filetypes": ["java", "c", "cpp", "objc", "objcpp", "html"],
            \ }

" JS
let g:syntastic_javascript_checkers = ['eslint']
let g:syntastic_javascript_eslint_exec = 'eslint_d'

" Python 3
let g:syntastic_python_flake8_exec = 'python3'
let g:syntastic_python_flake8_args = ['-m', 'flake8']

" YAML
let g:syntastic_yaml_checkers = ["yamllint"]

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ale
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:ale_sign_column_always=1
let g:ale_set_highlights=0
let g:ale_set_signs=1
let g:ale_lint_on_text_changed=1
let g:ale_lint_on_enter=0
let g:ale_lint_on_save=1
let g:ale_lint_on_filetype_changed=1

let g:ale_linters = {
    \ "jsx": ["eslint"],
    \ "c": [],
    \ "cpp": [],
    \ "clojure": ["joker"],
    \ }
let g:ale_python_flake8_executable = 'python3'
let g:ale_python_flake8_options = '-m flake8'

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" detectindent
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:detectindent_max_lines_to_analyse = 1024
let g:detectindent_autodetect = 1
let g:detectindent_preferred_indent = 2
let g:detectindent_preferred_expandtab = 1
let g:detectindent_min_indent = 2
" hope to $DEITY that no one uses > 4 indents
let g:detectindent_max_indent = 4

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" bufkill.vim
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:BufKillCreateMappings = 0

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" vim-cljfmt
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:clj_fmt_autosave = 0

augroup MyVimrc
  au FileType clojure nnoremap <buffer> <leader>r :Require<CR>
  au FileType clojure nnoremap <buffer> <leader>R :Require!<CR>
  au FileType clojure nnoremap <buffer> <leader>f :Cljfmt<CR>
augroup END

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" synesthesia
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:loaded_synesthesia=1
let g:synesthesia_banned_console_colors = []

" solarized-dark
for i in range(18, 256 + 1)
  call add(g:synesthesia_banned_console_colors, i)
endfor
" these are just cyclic repeats
for i in range(9, 14 + 1)
  call add(g:synesthesia_banned_console_colors, i)
endfor
" this is same as background color, don't want things to be invisible
call add(g:synesthesia_banned_console_colors, 0)

" this is the solarized color table
let s:color_table = [
    \ "002b36",
    \ "073642",
    \ "586e75",
    \ "657b83",
    \ "839496",
    \ "93a1a1",
    \ "eee8d5",
    \ "fdf6e3",
    \ "dc322f",
    \ "cb4b16",
    \ "b58900",
    \ "859900",
    \ "2aa198",
    \ "268bd2",
    \ "6c71c4",
    \ "d33682",
    \ ]
let g:synesthesia_gui_color_table = {
    \ 0 : s:color_table[0],
    \ 1 : s:color_table[8],
    \ 2 : s:color_table[11],
    \ 3 : s:color_table[10],
    \ 4 : s:color_table[13],
    \ 5 : s:color_table[14],
    \ 6 : s:color_table[12],
    \ 7 : s:color_table[5],
    \ 8 : s:color_table[3],
    \ 9 : s:color_table[8],
    \ 10 : s:color_table[11],
    \ 11 : s:color_table[10],
    \ 12 : s:color_table[13],
    \ 13 : s:color_table[14],
    \ 14 : s:color_table[12],
    \ 15 : s:color_table[7],
    \ 16 : s:color_table[9],
    \ 17 : s:color_table[15],
    \ }

let g:synesthesia_ignored_filetypes = []
call add(g:synesthesia_ignored_filetypes, '')
call add(g:synesthesia_ignored_filetypes, 'diff')
call add(g:synesthesia_ignored_filetypes, 'gitcommit')
call add(g:synesthesia_ignored_filetypes, 'help')
call add(g:synesthesia_ignored_filetypes, 'html')
call add(g:synesthesia_ignored_filetypes, 'markdown')
call add(g:synesthesia_ignored_filetypes, 'svn')
call add(g:synesthesia_ignored_filetypes, 'tex')
call add(g:synesthesia_ignored_filetypes, 'text')
call add(g:synesthesia_ignored_filetypes, 'xml')

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" vim-easymotion
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:EasyMotion_do_mapping = 0 " Disable default mappings
let g:EasyMotion_do_shade = 1
let g:EasyMotion_smartcase = 1
let g:EasyMotion_keys = "dsaklghqwertyuiopzxcvbnmfj"

" easymotion highlight colors
" hi link EasyMotionTarget Error
" hi EasyMotionTarget2First ctermbg=none ctermfg=46
" hi EasyMotionTarget2Second ctermbg=none ctermfg=46
" hi link EasyMotionShade Comment

nmap s <Plug>(easymotion-s2)
nmap S <Plug>(easymotion-overwin-f2)

" map <Leader>f <Plug>(easymotion-bd-fl)
" nmap <Leader>f <Plug>(easymotion-overwin-f)

" Move to line
" map <Leader>L <Plug>(easymotion-bd-jk)
" nmap <Leader>L <Plug>(easymotion-overwin-line)

" Move to word
" conflicts with my save
" map <Leader>w <Plug>(easymotion-bd-w)
" nmap <Leader>w <Plug>(easymotion-overwin-w)

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" scrollfix
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" scrolloff 0 is needed by scrollfix
set scrolloff=0
let g:scrollfix=50

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" vim-slime
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:slime_no_mappings = 1
let g:slime_target="tmux"
let g:slime_default_config = {"socket_name": "default", "target_pane": "1"}

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" vim-expand-region
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
vmap v <Plug>(expand_region_expand)
vmap <C-v> <Plug>(expand_region_shrink)

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" vim-asterisk
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:asterisk#keeppos=1
" map *  <Plug>(asterisk-z*)
" map #  <Plug>(asterisk-z#)
" map g* <Plug>(asterisk-gz*)
" map g# <Plug>(asterisk-gz#)


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" incsearch.vim
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" map /  <Plug>(incsearch-forward)\v
" map ?  <Plug>(incsearch-backward)\v
" map g/ <Plug>(incsearch-stay)\v

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" is.vim + friends
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
map n <Plug>(anzu-n-with-echo)
map N <Plug>(anzu-N-with-echo)

map *  <Plug>(asterisk-z*)
map g* <Plug>(asterisk-gz*)
map #  <Plug>(asterisk-z#)
map g# <Plug>(asterisk-gz#)

map / <Plug>(incsearch-forward)\v
map ? <Plug>(incsearch-backward)\v
map g/ <Plug>(incsearch-stay)\v

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" tagbar
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" usually the files that I deal with have functions arranged in some sort of
" logical grouping that we wnat to preserve
let g:tagbar_sort=0
let g:tagbar_width=40

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" vim-clang-format
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:clang_format#code_style="google"
let g:clang_format#detect_style_file=1
autocmd FileType c,cpp,objc nnoremap <buffer><Leader>f :<C-u>ClangFormat<CR>zz
autocmd FileType c,cpp,objc vnoremap <buffer><Leader>f :ClangFormat<CR>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" csscomplete.vim
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
autocmd FileType css set omnifunc=csscomplete#CompleteCSS noci

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" vim-jsx
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:jsx_ext_required = 0

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" vim-tmux-navigator
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:tmux_navigator_no_mappings = 1
nnoremap <silent> <C-h> :TmuxNavigateLeft<cr>
nnoremap <silent> <C-j> :TmuxNavigateDown<cr>
nnoremap <silent> <C-k> :TmuxNavigateUp<cr>
nnoremap <silent> <C-l> :TmuxNavigateRight<cr>

nnoremap <silent> <Left> :TmuxNavigateLeft<cr>
nnoremap <silent> <Right> :TmuxNavigateRight<cr>
nnoremap <silent> <Up> :TmuxNavigateUp<cr>
nnoremap <silent> <Down> :TmuxNavigateDown<cr>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" vim-startify
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:startify_session_dir="~/sessions/"
let g:startify_session_number = 32
let g:startify_files_number = 64

let g:startify_lists = [
    \ { 'type': 'sessions',  'header': ['   Sessions']       },
    \ { 'type': 'dir',       'header': ['   MRU '. getcwd()] },
    \ { 'type': 'files',     'header': ['   MRU']            },
    \ { 'type': 'bookmarks', 'header': ['   Bookmarks']      },
    \ { 'type': 'commands',  'header': ['   Commands']       },
    \ ]

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" vim-dirvish
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
augroup my_dirvish_events
  autocmd!
  " sort: folders at top, alphabetical, case-insensitive.
  " let g:dirvish_mode = ':sort ir /^.*[^\/]$/'
  let g:dirvish_mode = 1
  let g:dirvish_relative_paths = 1

  " Map `gr` to reload.
  autocmd FileType dirvish nnoremap <silent><buffer> gr :<C-U>Dirvish %<CR>

  " Map `gh` to hide dot-prefixed files.  Press `R` to "toggle" (reload).
  autocmd FileType dirvish nnoremap <silent><buffer> gh :silent keeppatterns g@\v/\.[^\/]+/?$@d _<cr>
augroup END

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" vim-fugitive
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
nnoremap <leader>gs :Gstatus<CR>
nnoremap <leader>gw :Gwrite<CR>
nnoremap <leader>gc :Gcommit --verbose<CR>
nnoremap <leader>gp :Gpush<CR>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" rainbow
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:rainbow_active=1
let g:rainbow_conf = {
\ 'guifgs': ['#008800', '#ff00ff', '#ffff00', '#00ff00', '#0088ff', '#ffffff'],
\ 'ctermfgs': ['lightblue', 'lightyellow', 'lightcyan', 'lightmagenta'],
\ 'guis': [''],
\ 'cterms': [''],
\ 'operators': '_,_',
\ 'contains_prefix': 'TOP',
\ 'parentheses_options': '',
\ 'parentheses': ['start=/(/ end=/)/ fold', 'start=/\[/ end=/\]/ fold', 'start=/{/ end=/}/ fold'],
\ 'separately': {
\   '*': {},
\   'javascript.jsx': {},
\   'plasmaplace': 0,
\   'markdown': {
\     'parentheses_options': 'containedin=markdownCode contained',
\   },
\   'lisp': {
\     'guifgs': ['royalblue3', 'darkorange3', 'seagreen3', 'firebrick', 'darkorchid3'],
\   },
\   'haskell': {
\     'parentheses': ['start=/(/ end=/)/ fold', 'start=/\[/ end=/\]/ fold', 'start=/\v\{\ze[^-]/ end=/}/ fold'],
\   },
\   'tex': {
\     'parentheses_options': 'containedin=texDocZone',
\     'parentheses': ['start=/(/ end=/)/', 'start=/\[/ end=/\]/'],
\   },
\   'vim': {
\     'parentheses_options': 'containedin=vimFuncBody,vimExecute',
\     'parentheses': ['start=/(/ end=/)/', 'start=/\[/ end=/\]/', 'start=/{/ end=/}/ fold'],
\   },
\   'xml': {
\     'syn_name_prefix': 'xmlRainbow',
\     'parentheses': ['start=/\v\<\z([-_:a-zA-Z0-9]+)(\s+[-_:a-zA-Z0-9]+(\=("[^"]*"|'."'".'[^'."'".']*'."'".'))?)*\>/ end=#</\z1># fold'],
\   },
\   'xhtml': {
\     'parentheses': ['start=/\v\<\z([-_:a-zA-Z0-9]+)(\s+[-_:a-zA-Z0-9]+(\=("[^"]*"|'."'".'[^'."'".']*'."'".'))?)*\>/ end=#</\z1># fold'],
\   },
\   'html': {
\     'parentheses': ['start=/\v\<((script|style|area|base|br|col|embed|hr|img|input|keygen|link|menuitem|meta|param|source|track|wbr)[ >])@!\z([-_:a-zA-Z0-9]+)(\s+[-_:a-zA-Z0-9]+(\=("[^"]*"|'."'".'[^'."'".']*'."'".'|[^ '."'".'"><=`]*))?)*\>/ end=#</\z1># fold'],
\   },
\   'perl': {
\     'syn_name_prefix': 'perlBlockFoldRainbow',
\   },
\   'php': {
\     'syn_name_prefix': 'phpBlockRainbow',
\     'contains_prefix': '',
\     'parentheses': ['start=/(/ end=/)/ containedin=@htmlPreproc contains=@phpClTop', 'start=/\[/ end=/\]/ containedin=@htmlPreproc contains=@phpClTop', 'start=/{/ end=/}/ containedin=@htmlPreproc contains=@phpClTop', 'start=/\v\<((area|base|br|col|embed|hr|img|input|keygen|link|menuitem|meta|param|source|track|wbr)[ >])@!\z([-_:a-zA-Z0-9]+)(\s+[-_:a-zA-Z0-9]+(\=("[^"]*"|'."'".'[^'."'".']*'."'".'|[^ '."'".'"><=`]*))?)*\>/ end=#</\z1># fold contains_prefix=TOP'],
\   },
\   'stylus': {
\     'parentheses': ['start=/{/ end=/}/ fold contains=@colorableGroup'],
\   },
\   'css': 0,
\   'sh': 0,
\ }
\}
augroup MyRaindowLoader
  auto Filetype * call rainbow_main#load()
	auto colorscheme * call rainbow_main#load()
augroup end

" }}}
" filetype specific settings {{{
function! MyJsonFormatter()
  let view = winsaveview()
  execute "%!python -m json.tool"
  call winrestview(view)
endfunction
function! MyJavascriptFormatter()
  let view = winsaveview()
  execute "%!prettier --parser babel --trailing-comma es5"
  call winrestview(view)
endfunction
function! MyScssFormatter()
  let view = winsaveview()
  execute "%!prettier --parser scss --trailing-comma es5"
  call winrestview(view)
endfunction
function! MyPythonFormatter()
  let view = winsaveview()
  execute "%!black -q -"
  call winrestview(view)
endfunction
function! MyGoFormatter()
  let view = winsaveview()
  execute "%!gofmt"
  call winrestview(view)
endfunction
augroup MyVimrc
  au BufWritePost *.vimrc source $MYVIMRC
  au BufWritePost *.gvimrc source $MYGVIMRC

  au FileType gitcommit setlocal foldlevel=9001

  " au BufNewFile,BufRead *.py setlocal foldmethod=syntax foldlevel=1
  au BufNewFile,BufRead *.py setlocal nofoldenable
  au BufNewFile,BufRead *.py setlocal omnifunc=pythoncomplete#Complete

  au FileType cmake setlocal commentstring=#\ %s

  au FileType dosbatch setlocal ff=dos
  au FileType dosbatch setlocal commentstring=REM\ %s

  au FileType Makefile setlocal noexpandtab

  au BufReadPost *.hlsl set filetype=fx
  
  au FileType css,less,scss setlocal iskeyword+=-
  au FileType javascript setlocal iskeyword+=$
  au FileType javascript setlocal cinoptions=g0,N-s,(0,u0,Ws,l1,j1,J1
  autocmd FileType json nnoremap <buffer> <Leader>f :call MyJsonFormatter()<CR>
  autocmd FileType javascript nnoremap <buffer> <Leader>f :call MyJavascriptFormatter()<CR>
  autocmd FileType javascript.jsx nnoremap <buffer> <Leader>f :call MyJavascriptFormatter()<CR>
  autocmd FileType scss nnoremap <buffer> <Leader>f :call MyScssFormatter()<CR>
  autocmd FileType python nnoremap <buffer> <Leader>f :call MyPythonFormatter()<CR>
  autocmd FileType go nnoremap <buffer> <Leader>f :call MyGoFormatter()<CR>
  
  au FileType markdown setlocal textwidth=80

  let g:pyindent_open_paren = '&sw'
  let g:pyindent_nested_paren = '&sw'
  let g:pyindent_continue = '&sw'
augroup END
" }}}
" Projects {{{
" no tags by default, omegacomplete is usually enough
set tags=

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" UNIX
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! FindAndRunMakefile()
  let prev_dir = ''
  let current_dir = expand('%:p:h')

  let max_search = 0

  while current_dir != prev_dir && current_dir != '/'
    let max_search += 1

    let makefile = current_dir . '/Makefile'
    if filereadable(makefile)
      let make_cmd = "make -f Makefile " . '-C ' . current_dir
      exe "botright term " . make_cmd
      resize 25
      nnoremap <buffer> q :q<cr>
      return
    endif

    let prev_dir = current_dir
    let current_dir = simplify(current_dir . '/..')
    if max_search == 8
      break
    endif
  endwhile
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Windows
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" function! IssueBuildCommandToVisualStudio()
"   let ahk_file = s:unix_home.'/dot/bin/issue_build_command_to_visual_studio.ahk'
"   if !filereadable(ahk_file)
"     return
"   endif

"   exe "silent! !".ahk_file
" endfunction

if has('unix')
  nnoremap <leader>m :update<CR>:call FindAndRunMakefile()<CR>
" elseif has('win32')
"   nnoremap <leader>m :update<CR>:call IssueBuildCommandToVisualStudio()<CR>
endif

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" random stuff
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" converts underscore_case to camelCase
" nnoremap <leader>c :s#_\(\l\)#\u\1#<CR>
vnoremap <leader>c :s#_\(\l\)#\u\1#<CR>

" }}}
