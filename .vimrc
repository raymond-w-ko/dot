" vim:fdm=marker

" when re-sourcing with this set, syntax highlighting changes!
" having an existence of .vimrc already implies this
"set nocompatible

augroup MyVimrc
  au!
augroup END

" http://utf8everywhere.org/
set encoding=utf-8

" pathogen {{{
let g:pathogen_disabled = []

" omegacomplete
let g:omegacomplete_version_preference = 1
if g:omegacomplete_version_preference == 2
  if has('java')
    call add(g:pathogen_disabled, "omegacomplete")
  else
    call add(g:pathogen_disabled, "omegacomplete2")
  endif
elseif g:omegacomplete_version_preference == 1
    call add(g:pathogen_disabled, "omegacomplete2")
endif
if has('java')
  let jar_list = split(globpath(expand('$HOME') . '/java', '*.jar'), "\n")
  call insert(jar_list, expand('$VIMRUNTIME') . '/vim.jar', 0)
  let jars = substitute(join(jar_list, ';'), '\\', '/', 'g')
  let jars = substitute(jars, ' ', '\\ ', 'g')
  exe "set javacp=" . jars
  javarepl clojure
endif

" if this version of vim doesn't have Pythong bindings
if !has('python')
  call add(g:pathogen_disabled, "omegacomplete")
  call add(g:pathogen_disabled, "omegacomplete2")
  call add(g:pathogen_disabled, "ultisnips")
endif

" very heavy, adds over 9000 keywords and library functions
call add(g:pathogen_disabled, "cocoa.vim")
" disable this for now, try out dimmed out parentheses
call add(g:pathogen_disabled, "vim-niji")
" disable this for now, try out lexima.vim
call add(g:pathogen_disabled, "vim-endwise")
" disable lexima.vim due to deletion limitations
call add(g:pathogen_disabled, "lexima.vim")

" call add(g:pathogen_disabled, "FastFold")
" call add(g:pathogen_disabled, "PHP-Indenting-for-VIm")
" call add(g:pathogen_disabled, "a.vim")
" call add(g:pathogen_disabled, "cocoa.vim")
" call add(g:pathogen_disabled, "ctrlp-py-matcher")
" call add(g:pathogen_disabled, "ctrlp.vim")
" call add(g:pathogen_disabled, "detectindent")
" call add(g:pathogen_disabled, "gundo.vim")
" call add(g:pathogen_disabled, "scratch")
" call add(g:pathogen_disabled, "scrollfix")
" call add(g:pathogen_disabled, "syntastic")
" call add(g:pathogen_disabled, "toggle_words")
" call add(g:pathogen_disabled, "vim-airline")
" call add(g:pathogen_disabled, "vim-bufkill")
" call add(g:pathogen_disabled, "vim-capslock")
" call add(g:pathogen_disabled, "vim-characterize")
" call add(g:pathogen_disabled, "vim-classpath")
" call add(g:pathogen_disabled, "vim-cljfmt")
" call add(g:pathogen_disabled, "vim-clojure-static")
" call add(g:pathogen_disabled, "vim-commentary")
" call add(g:pathogen_disabled, "vim-cpp")
" call add(g:pathogen_disabled, "vim-dispatch")
" call add(g:pathogen_disabled, "vim-easy-align")
" call add(g:pathogen_disabled, "vim-easymotion")
" call add(g:pathogen_disabled, "vim-endwise")
" call add(g:pathogen_disabled, "vim-fireplace")
" call add(g:pathogen_disabled, "vim-fugitive")
" call add(g:pathogen_disabled, "vim-haystack")
" call add(g:pathogen_disabled, "vim-lua-indent")
" call add(g:pathogen_disabled, "vim-markdown")
" call add(g:pathogen_disabled, "vim-niji")
" call add(g:pathogen_disabled, "vim-obsession")
" call add(g:pathogen_disabled, "vim-pathogen")
" call add(g:pathogen_disabled, "vim-repeat")
" call add(g:pathogen_disabled, "vim-rsi")
" call add(g:pathogen_disabled, "vim-salve")
" call add(g:pathogen_disabled, "vim-scriptease")
" call add(g:pathogen_disabled, "vim-sensible")
" call add(g:pathogen_disabled, "vim-sexp-mappings-for-regular-people")
" call add(g:pathogen_disabled, "vim-sexp")
" call add(g:pathogen_disabled, "vim-slamhound")
" call add(g:pathogen_disabled, "vim-slime")
" call add(g:pathogen_disabled, "vim-surround")
" call add(g:pathogen_disabled, "vim-synesthesia")
" call add(g:pathogen_disabled, "vim-tbone")
" call add(g:pathogen_disabled, "vim-tmux-navigator")
" call add(g:pathogen_disabled, "vim-unimpaired")
" call add(g:pathogen_disabled, "vim-vinegar")
" call add(g:pathogen_disabled, "vim-yankstack")

runtime bundle/vim-pathogen/autoload/pathogen.vim
call pathogen#infect()
call pathogen#helptags()
"}}}
" General {{{

" load sensible defaults by our prophet Tim Pope
runtime! plugin/sensible.vim

if has('win32')
  let s:data_dir="$APPDATA/Vim"
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
set history=10000
set lazyredraw
set showcmd
set ttyfast
set matchtime=0
set splitbelow
set splitright
set title
set showtabline=2
set cmdheight=1
set completeopt+=menu
set completeopt+=menuone
set completeopt+=preview
set pumheight=16
set autochdir
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
set ttymouse=xterm2
set clipboard=autoselect
set pastetoggle=<F9>

" this breaks vim-capslock since you can't press <C-g>c in less than 100ms
" augroup MyVimrc
"   autocmd InsertEnter * set timeoutlen=100
"   autocmd InsertLeave * set timeoutlen=750
" augroup END

" timeout is needed due to completing fj with omegacomplete
set timeout
set timeoutlen=750
set ttimeout
set ttimeoutlen=100    " needed to avoid leaving insert mode delay for vim-airline

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

" binaries with a 99.9% chance of not being edited
set wildignore+=*.exe,*.dll
" media files in a binary format
set wildignore+=*.jpg,*.jpeg,*.png,*.gif,*.bmp,*.tga,*.mp3,*.ico,*.wav
set wildignore+=*.bik,*.ani,*.mask,*.dds
" version control directories
" adding .git breaks vim-fugitive
"set wildignore+=.hg,.git,.svn
" Visual Studio files
set wildignore+=*.ncb,*.suo,*.user,*.vcproj,*.vcxproj,*.out,*.sln,*.pdb
set wildignore+=*.manifest,*.dep,*.idb,*.ipch,*.o,*.obj
" GCC
set wildignore+=*.d,*.a
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

let s:project_directories_list = [
    \ '__UNIX_HOME__/src/ocularwm',
    \ '__UNIX_HOME__/src/windmenu',
    \ '__UNIX_HOME__/src/dk2test',
    \ '__UNIX_HOME__/src/tsukuyomi',
    \ '__UNIX_HOME__/src/letterdungeon',
    \ '__UNIX_HOME__/src/vim/src',
    \ '__UNIX_HOME__/.vim/bundle/omegacomplete',
    \ '__UNIX_HOME__/dot/.vim/bundle/omegacomplete',
    \ '__SVN__/SVN/Syandus_ALIVE3/Frameworks/Carbon',
    \ '__SVN__/SVN/Syandus_ALIVE3/Frameworks/CarbonCME',
    \ '__SVN__/SVN/Syandus_ALIVE3/Frameworks/Oxygen',
    \ '__SVN__/SVN/Syandus_ALIVE3/Groundhog/Client',
    \ '__SVN__/SVN/Syandus_ALIVE3/Groundhog/ConnectionTester',
    \ '__SVN__/SVN/Syandus_ALIVE3/Groundhog/Server',
    \ '__SVN__/SVN/Syandus_ALIVE3/Groundhog/Shared',
    \ '__SVN__/SVN/Syandus_ALIVE3/Hub/Source',
    \ '__SVN__/SVN/Syandus_ALIVE3/Hub/Web',
    \ '__SVN__/SVN/Syandus_ALIVE3/Hub/Web/galleries/cme',
    \ '__SVN__/SVN/Syandus_ALIVE3/Installation Suite/trunk',
    \ '__SVN__/SVN/Syandus_ALIVE3/Mac/trunk/ALIVE Med',
    \ '__SVN__/SVN/Syandus_ALIVE3/Metrics/SyLoginParser',
    \ '__SVN__/SVN/Syandus_ALIVE3/Metrics/SyMetrics',
    \ '__SVN__/SVN/Syandus_ALIVE3/Metrics/web',
    \ '__SVN__/SVN/Syandus_ALIVE3/Platform/Source/Code',
    \ '__SVN__/SVN/Syandus_ALIVE3/Tools/Source/Launcher',
    \ '__SVN__/SVN/Syandus_ALIVE3/Tools/Source/SyHandleGen',
    \ '__SVN__/SVN/Syandus_ALIVE3/Tools/Source/SyRefresh',
    \ '__SVN__/SVN/Syandus_ALIVE4/Frameworks/Carbon',
    \ '__SVN__/SVN/Syandus_ALIVE4/Frameworks/Oxygen',
    \ '__SVN__/SVN/Syandus_ALIVE4/Frameworks/Hydrogen',
    \ '__SVN__/SVN/Syandus_ALIVE4/Frameworks/Nitrogen',
    \ '__SVN__/SVN/Syandus_ALIVE4/Frameworks/Nitrogen16',
    \ '__SVN__/SVN/Syandus_ALIVE4/Platform/Source/Code',
    \ '__SVN__/SVN/Syandus_ALIVE4/Tools/Source/SyProjectGenerator',
    \ '__SVN__/SVN/Syandus_ALIVE4/Tools/Source/mercky',
    \ '__SVN__/SVN/Syandus_ALIVE4/Web/Merck/Phase 1/PCRD/retroSyrus',
    \ '__SVN__/SVN/Syandus_Company/Web/Syandus.com/main/2012-html',
    \ '__SVN__/SVN/Syandus_Company/Web/Syandus.com/main/2013-html/html',
    \ '__SVN__/SVN/Syandus_Cores/C_CMSC_MS_01',
    \ '__SVN__/SVN/Syandus_Cores/C_ImmunoSim_01',
    \ '__SVN__/SVN/Syandus_Cores/C_MS_PatientEd_01',
    \ '__SVN__/SVN/Syandus_Cores/C_Hemo_PatientEd_01',
    \ '__SVN__/SVN/Syandus_Cores/C_MS_Treatment_01',
    \ '__SVN__/SVN/Syandus_Cores/C_MM_Treatment_01',
    \ '__SVN__/SVN/Syandus_Cores/C_mCRC_Treatment_01',
    \ '__SVN__/SVN/Syandus_Cores/C_Mic_HTN_01',
    \ '__SVN__/SVN/Syandus_Cores/C_Ogre_Lair_01',
    \ '__SVN__/SVN/Syandus_Cores/C_Spv_COPD_01',
    \ '__SVN__/SVN/Syandus_Cores/C_Sut_AE_01',
    \ '__SVN__/SVN/Syandus_Cores/C_Sym_DM_01',
    \ '__SVN__/SVN/Syandus_Cores/C_Unb_COPD_01',
    \ '__SVN__/SVN/Syandus_ALIVE4/Cellulose',
    \ '__SVN__/SVN/Syandus_ALIVE5/nitrogen',
    \ ]

let g:my_project_directories = {}
for directory in s:project_directories_list
  let dir = MyTranslateDirectory(directory)
  let g:my_project_directories[dir] = 1
endfor

" traverse up parent directories until it finds one that matches in the above
" list
function! MyGetProjectDirectory()
  let last_directory = ''
  let directory = getcwd()

  while has_key(g:my_project_directories, directory) == 0 && last_directory != directory
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

function! SaveAndCheckIfModified()
  if &modified && !&readonly && len(bufname('%')) > 0
    update
    " too distracting (flickering) and slow
    "SyntasticCheck
  endif
endfunction

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
if !exists("g:already_set_color_scheme") && !($TERM == "linux")
    set background=dark

    let base16colorspace=256
    colorscheme base16-solarized

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
set fillchars+=vert:\|

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

" mutally exclusive I think
"set scrolloff=9999
"nnoremap <silent> zz :call CenterCursorAesthetically()<CR>
"nmap n nzzzv
"nmap N Nzzzv
"nmap G Gzz
"nnoremap g; g;zz
"nnoremap g, g,zz

" Don't move on *, superseded by vim-asterisk
"nnoremap * *<c-o>
" nnoremap <silent> * :set nohls<CR>:let @/='\C\<<C-R>=expand('<cword>')<CR>\>'<CR>:set hls<CR>

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
nnoremap <silent> j gj
nnoremap <silent> k gk

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
" Folding {{{
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
"onoremap id i[
"onoremap ad a[
"vnoremap id i[
"vnoremap ad a[

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
" greek small letter nu
inoremap ν <Esc>
cnoremap ν <C-c>

" cnoremap jj <C-c>
" inoremap jj <Esc>
" inoremap <silent> jj <C-r>=lexima#insmode#escape()<CR><Esc>


" mirror dd and D, a bit hard to get use to
try
  call yankstack#setup()
catch
endtry
map Y y$

" looking at junegunn's vimrc, <C-f> and <C-b> are usually overkill
" not if using scrollfix though...
"noremap <C-F> <C-D>
"noremap <C-B> <C-U>

" lazyness, and to help me use Ex commands
" since I am testing crazy semicolon mode map layout, this is not necessary
" nnoremap ; :
" nnoremap : ;
" vnoremap ; :
" vnoremap : ;

" fastest way to save a file
nnoremap <silent> <leader>w :wall<CR>

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
nnoremap <C-BS> :BD<CR>
nnoremap  :BD<CR>

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

function! s:MyBasicCR()
  
  let keys = ""
  if pumvisible()
    let keys .= "\<C-e>"
  endif
  
  return keys . "\<CR>"
endfunction

function! s:SetupPairBindings()
  " handled by vim-sexp
  if &ft == 'clojure' || &ft == 'lisp' || &ft == 'scheme'
    exe "imap <buffer> φ ("
    exe "imap <buffer> σ {"
    exe "imap <buffer> ρ ["
    exe 'imap <buffer> θ "'
    inoremap <buffer> <CR> <C-r>=<SID>MyBasicCR()<CR>
  else
    " semimap helpers
    inoremap <buffer> φ ()<C-g>U<Left>
    inoremap <buffer> σ {}<C-g>U<Left>
    inoremap <buffer> ρ []<C-g>U<Left>
    inoremap <buffer> θ ""<C-g>U<Left>
    inoremap <buffer> <BS> <C-r>=<SID>EmptyPairDeleterBackspace()<CR>
    inoremap <buffer> <CR> <C-r>=<SID>MySmarterCR()<CR>
  endif
endfunction

augroup MyVimrc
  au FileType * call <SID>SetupPairBindings()
augroup END

let s:move_right_keystroke = "\<C-g>U\<Right>"
let s:move_right_pair_ends = { "'" : 1, '"' : 1, ')' : 1, ']' : 1, '}' : 1 }
function! s:PareditForwardUp()
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

inoremap <expr> χ <SID>PareditForwardUp()

" Platform specific keybinds
if has("unix")
    cmap w!! w !sudo tee % >/dev/null

    nnoremap <leader>ev :e ~/.vimrc<CR>
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
endif

function! FindFileInProjectDirectory()
    execute ':CtrlP ' . EscapePathname(MyGetProjectDirectory())
endfunction
nnoremap <leader>p :call FindFileInProjectDirectory()<CR>

nnoremap <leader>b :CtrlPBuffer<CR>

function! MyAlternateFunction()
    let old_buf_nr = bufnr('%')
    A
    let new_buf_nr = bufnr('%')
    "if (old_buf_nr != new_buf_nr)
        "call CenterCursorAesthetically()
    "endif
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
nnoremap <silent> <Left> :call MarkWindowSwap()<CR><C-w>h:call DoWindowSwap()<CR>
nnoremap <silent> <Right> :call MarkWindowSwap()<CR><C-w>l:call DoWindowSwap()<CR>

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
  let num_vsplits = (&columns / (80 - 1)) - 1

  " get the current directory because we want to replicate this
  " in the new tab
  let current_directory = expand("%:p:h")

  if winnr('$') > 1
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
endfunction
" nnoremap <leader>wt :call CreateAndSetupVsplits()<CR>
" nnoremap <leader>ww :tabclose<CR>

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

augroup MyVimrc
  " only show cursorline if a window has focus
  " this noticably slows down VIM in files with complicated syntax hilighting,
  " like PHP, so disable it for now.
  " au VimEnter,WinEnter,BufWinEnter * setlocal cursorline
  " au WinLeave * setlocal nocursorline

  "au VimEnter,WinEnter,BufWinEnter * setlocal cursorcolumn
  "au WinLeave * setlocal nocursorcolumn

  " hack for console VIM so that check for changed files work correctly
  au FocusGained,BufEnter * if !has("gui_running") | :silent! checktime | endif

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

  " this probably causes more trouble than it is worth, especially for files not
  " under version control
  " but I am to lazy and often don't want to press Enter to save...
  "au InsertLeave * call SaveAndCheckIfModified()

  " generates too many annoying deltas in open source projects like OGRE
  "au BufWritePre C:/SVN/* call StripTrailingWhitespace()
  "au BufWritePre *.h,*.hpp,*.c,*.cc,*.cpp,*.java,*.py,*.lua call StripTrailingWhitespace()

  " help in new tab to avoid interfering with existing tab layout
  autocmd BufEnter *.txt call s:SetupHelpTab()

  " de-emphasized parentheses
  au BufReadPost * highlight link SubtleParentheses Comment
  au BufReadPost * syntax match SubtleParentheses /(\|)/
  au FileType clojure,lisp,scheme syntax match SubtleParentheses /\[\|\]\|{\|}/
augroup END

" }}}
" Plugins {{{

" let g:loaded_matchparen = 1

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" a.vim
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:alternateNoDefaultAlternate=1

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" NetRW
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"let g:netrw_silent=1
" apparently enabling this hijacks the mouse completely
" so you can't use it to select stuff (WTF!)
"let g:netrw_mousemaps=0
"let g:netrw_cygwin = 1

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" CtrlP
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:ctrlp_map = "<C-\\>"           " set to something that I will never use
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

let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/]\.(git|hg|svn)$|[\/]compiled[\/]out$',
  \ }

if executable('ag')
  set grepprg=ag\ --nogroup\ --nocolor
  let exts=&wildignore
  let exts .= ',.git,.hg,.svn'
  let ignored_exts = map(split(exts, ','), '"--ignore \"" . v:val . "\""')
  let ignore_string = join(ignored_exts, ' ')
  let g:ctrlp_user_command = 'ag %s -i --nocolor --nogroup --hidden -g "" ' . ignore_string
endif

if !has('python')
  echo 'In order to use pymatcher plugin, you need +python compiled vim'
else
  let g:ctrlp_match_func = { 'match': 'pymatcher#PyMatch' }
endif

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" vim-powerline
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:Powerline_symbols='compatible'
let g:Powerline_stl_path_style='short'
let g:Powerline_theme='default'
let g:Powerline_colorscheme='default'

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" UltiSnips
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:UltiSnipsExpandTrigger = "<F1>"
let g:UltiSnipsListSnippets = "<C-F1>"
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

" these are default values that should be overriden if needed
let g:clojure_fuzzy_indent = 1
let g:clojure_fuzzy_indent_patterns = ['^with', '^def', '^let']
let g:clojure_fuzzy_indent_blacklist = ['-fn$', '\v^with-%(meta|out-str|loading-context)$']

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
"NERDCommenter
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:NERDCustomDelimiters = {
    \ 'syxml': { 'left': '//', 'right': ''},
    \ 'ogre': { 'left': '//', 'right': ''},
    \ 'glsl': { 'left': '//', 'right': ''}
    \ }

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" omegacomplete
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"let g:omegacomplete_log_file = "C:\\SVN\\omegacomplete.txt"
let g:omegacomplete_normal_hi_cmds=[
    \ "hi Pmenu guifg=#00ff00 guibg=#002b36 gui=none ctermbg=0 ctermfg=046 cterm=none",
    \ "hi PmenuSel guifg=#002b36 guibg=#00ff00 gui=none ctermbg=046 ctermfg=0 cterm=none",
    \ ]

let g:omegacomplete_corrections_hi_cmds=[
    \ "hi Pmenu guifg=#ffff00 guibg=#002b36 gui=none ctermbg=0 ctermfg=226 cterm=none",
    \ "hi PmenuSel guifg=#002b36 guibg=#ffff00 gui=none ctermbg=226 ctermfg=0 cterm=none",
    \ ]

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" syntastic
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:syntastic_enable_highlighting = 1
let g:syntastic_enable_balloons = 1
let g:syntastic_auto_jump = 3
let g:syntastic_enable_signs = 1
let g:syntastic_mode_map = {
            \ 'mode': 'active',
            \ 'active_filetypes': ['ruby', 'php', 'python'],
            \ 'passive_filetypes': ['java', 'c', 'cpp', 'objc', 'objcpp'] }

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" paredit
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:paredit_leader = ','
let g:paredit_shortmaps = 0

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
" airline
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:airline_left_sep=''
let g:airline_right_sep=''
let g:airline_theme='base16'
let g:airline#extensions#whitespace#enabled = 0
let g:airline#extensions#tabline#enabled = 1

" bufkill.vim
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
" base16 default colorscheme
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
" Turn on case insensitive feature
let g:EasyMotion_smartcase = 1

" Bi-directional find motion
" Jump to anywhere you want with minimal keystrokes, with just one key binding.
" `s{char}{label}`
"nmap s <Plug>(easymotion-s)
" or
" `s{char}{char}{label}`
" Need one more keystroke, but on average, it may be more comfortable.
nmap s <Plug>(easymotion-s2)
vmap s <Plug>(easymotion-s2)
omap z <Plug>(easymotion-s2)

omap f <Plug>(easymotion-bd-fl)
xmap f <Plug>(easymotion-bd-fl)
omap F <Plug>(easymotion-Fl)
xmap F <Plug>(easymotion-Fl)
omap t <Plug>(easymotion-tl)
xmap t <Plug>(easymotion-tl)
omap T <Plug>(easymotion-Tl)
xmap T <Plug>(easymotion-Tl)

" JK motions: Line motions
map <Leader>j <Plug>(easymotion-j)
map <Leader>k <Plug>(easymotion-k)


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" yankstack
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
nmap <C-p> <Plug>yankstack_substitute_older_paste
nmap <C-n> <Plug>yankstack_substitute_newer_paste

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" scrollfix
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:scrollfix=50
let g:scrollinfo=0

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" vim-slime
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:slime_target="tmux"
let g:slime_default_config = {"socket_name": "default", "target_pane": "1"}

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" lexima
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
augroup MyVimrc
  autocmd FileType clojure,lisp,scheme let b:lexima_disabled = 0
augroup END

let g:lexima_enable_basic_rules = 0
" call lexima#set_default_rules()

let s:lexima_rules = [
\ {'char': 'φ', 'input' : '(', 'input_after': ')'},
\ {'char': ')', 'at': '\%#)', 'leave': 1},
\ {'char': '<BS>', 'at': '(\%#)', 'delete': 1},
\ {'char': 'σ', 'input': '{', 'input_after': '}'},
\ {'char': '}', 'at': '\%#}', 'leave': 1},
\ {'char': '<BS>', 'at': '{\%#}', 'delete': 1},
\ {'char': 'ρ', 'input': '[', 'input_after': ']'},
\ {'char': ']', 'at': '\%#]', 'leave': 1},
\ {'char': '<BS>', 'at': '\[\%#\]', 'delete': 1},
\
\ {'char': 'θ', 'input' : '"', 'input_after': '"'},
\ {'char': '"', 'at': '\%#"', 'leave': 1},
\ {'char': '"', 'at': '^\s*\%#', 'filetype': 'vim'},
\ {'char': '"', 'at': '\%#\s*$', 'filetype': 'vim'},
\ {'char': '<BS>', 'at': '"\%#"', 'delete': 1},
\ {'char': '"', 'input' : '"', 'at': '\%#"""', 'leave': 3},
\ {'char': '<BS>', 'at': '"""\%#"""', 'input': '<BS><BS><BS>', 'delete': 3},
\
\ {'char': "'", 'input_after': "'"},
\ {'char': "'", 'at': '\%#''', 'leave': 1},
\ {'char': "'", 'at': '\w\%#''\@!'},
\ {'char': "'", 'at': '\\\%#'},
\ {'char': "'", 'at': '\\\%#', 'leave': 1, 'filetype': ['vim', 'sh', 'csh', 'ruby', 'tcsh', 'zsh']},
\ {'char': "'", 'filetype': ['haskell', 'lisp', 'clojure', 'ocaml', 'scala']},
\ {'char': '<BS>', 'at': "'\\%#'", 'delete': 1},
\ {'char': "'", 'at': "''\\%#", 'input_after': "'''"},
\ {'char': "'", 'at': "\\%#'''", 'leave': 3},
\ {'char': '<BS>', 'at': "'''\\%#'''", 'input': '<BS><BS><BS>', 'delete': 3},
\ {'char': '`', 'input_after': '`'},
\ {'char': '`', 'at': '\%#`', 'leave': 1},
\ {'char': '<BS>', 'at': '`\%#`', 'delete': 1},
\ {'char': '`', 'at': '``\%#', 'input_after': '```'},
\ {'char': '`', 'at': '\%#```', 'leave': 3},
\ {'char': '<BS>', 'at': '```\%#```', 'input': '<BS><BS><BS>', 'delete': 3},
\ ]

for rule in s:lexima_rules
  " call lexima#add_rule(rule)
endfor

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" vim-expand-region
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
vmap v <Plug>(expand_region_expand)
vmap <C-v> <Plug>(expand_region_shrink)

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" incsearch.vim
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
map /  <Plug>(incsearch-forward)\v
map ?  <Plug>(incsearch-backward)\v
map g/ <Plug>(incsearch-stay)\v

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" vim-asterisk
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:asterisk#keeppos=1
map *  <Plug>(asterisk-z*)
map #  <Plug>(asterisk-z#)
map g* <Plug>(asterisk-gz*)
map g# <Plug>(asterisk-gz#)

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
autocmd FileType c,cpp,objc nnoremap <buffer><Leader>f :<C-u>ClangFormat<CR>
autocmd FileType c,cpp,objc vnoremap <buffer><Leader>f :ClangFormat<CR>

" }}}
" filetype specific settings {{{
augroup MyVimrc
  au BufWritePost *.vimrc source $MYVIMRC
  au BufWritePost *.gvimrc source $MYGVIMRC

  au FileType gitcommit setlocal foldlevel=9001

  au BufNewFile,BufRead *.py setlocal foldmethod=syntax foldlevel=1
  au BufNewFile,BufRead *.py setlocal omnifunc=pythoncomplete#Complete
  
  au FileType cmake setlocal commentstring=#\ %s

  au FileType dosbatch setlocal ff=dos
  au FileType dosbatch setlocal commentstring=REM\ %s

  au FileType Makefile setlocal noexpandtab
augroup END
" }}}
" Projects {{{
" no tags by default, omegacomplete is usually enough
set tags=

    "\ 'Platform',           '__SVN__/SVN/Syandus_ALIVE3/Platform/Source/Code',
    "\ 'Carbon',             '__SVN__/SVN/Syandus_ALIVE3/Frameworks/Carbon',
    "\ 'CarbonCME',          '__SVN__/SVN/Syandus_ALIVE3/Frameworks/CarbonCME',
    "\ 'Hub',                '__SVN__/SVN/Syandus_ALIVE3/Hub/Source',
    "\ 'Metrics',            '__SVN__/SVN/Syandus_ALIVE3/Metrics',
let s:commands = [
    \ 'Omegacomplete',      '__UNIX_HOME__/dot/.vim/bundle/omegacomplete',
    \ 'Omegacomplete2',     '__UNIX_HOME__/dot/.vim/bundle/omegacomplete2',
    \ 'OcularWM',           '__UNIX_HOME__/src/ocularwm',
    \ 'Windmenu',           '__UNIX_HOME__/src/windmenu',
    \ 'Dk2test',            '__UNIX_HOME__/src/dk2test',
    \ 'Collimator',         '__UNIX_HOME__/src/collimator',
    \ 'Diffractor',         '__UNIX_HOME__/src/diffractor',
    \ 'LetterDungeon',      '__UNIX_HOME__/src/letterdungeon',
    \
    \ 'SVN',                '__SVN__/SVN/',
    \ 'Platform4',          '__SVN__/SVN/Syandus_ALIVE4/Platform/Source/Code',
    \ 'Doc4',               '__SVN__/SVN/Syandus_ALIVE4/Documentation',
    \ 'Carbon4',            '__SVN__/SVN/Syandus_ALIVE4/Frameworks/Carbon',
    \ 'Oxygen',             '__SVN__/SVN/Syandus_ALIVE4/Frameworks/Oxygen',
    \ 'Hydrogen',           '__SVN__/SVN/Syandus_ALIVE4/Frameworks/Hydrogen',
    \ 'Nitrogen',           '__SVN__/SVN/Syandus_ALIVE4/Frameworks/Nitrogen',
    \ 'Proton',             '__SVN__/SVN/Syandus_ALIVE4/Frameworks/Proton',
    \ 'Symlin',             '__SVN__/SVN/Syandus_Cores/C_Sym_DM_01',
    \ 'Spiriva',            '__SVN__/SVN/Syandus_Cores/C_Spv_COPD_01',
    \ 'Copd',               '__SVN__/SVN/Syandus_Cores/C_COPD_Treatment_01',
    \ 'ImmuneQuest',        '__SVN__/SVN/Syandus_Cores/C_ImmunoSim_01',
    \ 'MsPatientEd',        '__SVN__/SVN/Syandus_Cores/C_MS_PatientEd_01',
    \ 'HemoPatientEd',      '__SVN__/SVN/Syandus_Cores/C_Hemo_PatientEd_01',
    \ 'Treatment',          '__SVN__/SVN/Syandus_Cores/C_MS_Treatment_01',
    \ 'MmTreatment',        '__SVN__/SVN/Syandus_Cores/C_MM_Treatment_01',
    \ 'MCRC',               '__SVN__/SVN/Syandus_Cores/C_mCRC_Treatment_01',
    \ 'Sutent',             '__SVN__/SVN/Syandus_Cores/C_Sut_AE_01',
    \ 'SyMetrics',          '__SVN__/SVN/Syandus_ALIVE3/Metrics/SyMetrics',
    \ 'SyLogParser',        '__SVN__/SVN/Syandus_ALIVE3/Metrics/SyLoginParser',
    \ 'SyHandleGen',        '__SVN__/SVN/Syandus_ALIVE3/Tools/Source/SyHandleGen',
    \ 'SyHandleGen4',       '__SVN__/SVN/Syandus_ALIVE4/Tools/Source/SyHandleGen',
    \ 'Groundhog',          '__SVN__/SVN/Syandus_ALIVE3/Groundhog',
    \ 'GroundhogClient',    '__SVN__/SVN/Syandus_ALIVE3/Groundhog/Client',
    \ 'GroundhogServer',    '__SVN__/SVN/Syandus_ALIVE3/Groundhog/Server',
    \ 'GroundhogShared',    '__SVN__/SVN/Syandus_ALIVE3/Groundhog/Shared',
    \ 'ConnectionTester',   '__SVN__/SVN/Syandus_ALIVE3/Groundhog/ConnectionTester',
    \ 'SyRefresh',          '__SVN__/SVN/Syandus_ALIVE3/Tools/Source/SyRefresh',
    \ 'SyProjectGenerator', '__SVN__/SVN/Syandus_ALIVE4/Tools/Source/SyProjectGenerator',
    \ 'OgreLair',           '__SVN__/SVN/Syandus_Cores/C_Ogre_Lair_01',
    \ 'Ms',                 '__SVN__/SVN/Syandus_Cores/C_CMSC_MS_01',
    \ 'SyandusHtml5',       '__SVN__/SVN/Syandus_Company/Web/Syandus.com/main/2013-html/html',
    \ 'Cellulose',          '__SVN__/SVN/Syandus_ALIVE4/Cellulose',
    \ 'Rosettastone',       '__SVN__/SVN/Syandus_Web/Merck/rosettastone',
    \ 'Merck',              '__SVN__/SVN/Syandus_Web/Merck',
    \ 'Nitrogen5',           '__SVN__/SVN/Syandus_ALIVE5/nitrogen',
    \ ]
for i in range(len(s:commands) / 2)
    let cmd = (i * 2) + 0

    let dir = s:commands[(i * 2) + 1]
    let dir = MyTranslateDirectory(dir)

    let g:my_project_directories[dir] = 1

    exe 'com! ' . s:commands[cmd] . ' cd ' . dir
endfor

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
      echom current_dir
      execute "!make -f Makefile " . '-C ' . current_dir
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
function! IssueBuildCommandToVisualStudio()
  let ahk_file = s:unix_home.'/dot/bin/issue_build_command_to_visual_studio.ahk'
  if !filereadable(ahk_file)
    return
  endif
  
  exe "silent! !".ahk_file
endfunction

if has('unix')
  nnoremap <leader>m :update<CR>:call FindAndRunMakefile()<CR>
elseif has('win32')
  nnoremap <leader>m :update<CR>:call IssueBuildCommandToVisualStudio()<CR>
endif

" }}}
