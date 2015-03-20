" when re-sourcing with this set, syntax highlighting changes!
" having an existence of .vimrc already implies this
"set nocompatible

" don't customize anything if we are running in evim mode
if v:progname =~? "evim"
  finish
endif

" http://utf8everywhere.org/
set encoding=utf-8

if exists('+regexpengine')
    " automatic engine selection
    set regexpengine=0

    " use old engine (this could be faster in some cases to due new engine
    " still being a work in progress)

    "set regexpengine=1

    " use new engine, faster in particular tricky cases due to NFA
    "set regexpengine=2
end

" pathogen
let g:pathogen_disabled = []
call add(g:pathogen_disabled, "YankRing.vim")
call add(g:pathogen_disabled, "vim-easymotion")
call add(g:pathogen_disabled, "rainbow_parentheses.vim")
"call add(g:pathogen_disabled, "vim-fireplace")

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

if !has('python')
  call add(g:pathogen_disabled, "omegacomplete")
  call add(g:pathogen_disabled, "omegacomplete2")
  call add(g:pathogen_disabled, "ultisnips")
endif

if has('java')
  let jar_list = split(globpath(expand('$HOME') . '/java', '*.jar'), "\n")
  call insert(jar_list, expand('$VIMRUNTIME') . '/vim.jar', 0)
  let jars = substitute(join(jar_list, ';'), '\\', '/', 'g')
  let jars = substitute(jars, ' ', '\\ ', 'g')
  exe "set javacp=" . jars

  javarepl clojure
endif

runtime bundle/vim-pathogen/autoload/pathogen.vim
call pathogen#infect()
call pathogen#helptags()

" load sensible defaults by our prophet Tim Pope
runtime! plugin/sensible.vim

let s:data_dir = has('win32') ? '$APPDATA/Vim' : match(system('uname'), "Darwin") > -1 ? '~/Library/Vim' : empty($XDG_DATA_HOME) ? '~/.local/share/vim' : '$XDG_DATA_HOME/vim'
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

set fileformats=unix,dos,mac

if !has("gui_running")
  " need this otherwise colors disappear
  if !exists('g:has_set_my_console_vim_settings')
    set t_Co=256
    " Prevent Vim from clobbering the scrollback buffer. See
    " http://www.shallowsky.com/linux/noaltscreen.html
    "
    " this basically doesn't clear the screen when you close Vim
    "set t_ti= t_te=

    "set t_RV=
    "set ttymouse=
    "set ttymouse=xterm2
    "set mouse=
    let g:has_set_my_console_vim_settings = 1
  endif
endif

" General {{{
set autowrite
set autowriteall
set shortmess+=aIc    " no intro message, no ins-completion-menu
set report=0 " report back when greater than N lines changed
set showmode
set hidden
set novisualbell
set noerrorbells
augroup CursorLine
  au!
  au VimEnter,WinEnter,BufWinEnter * setlocal cursorline
  au WinLeave * setlocal nocursorline
  "au VimEnter,WinEnter,BufWinEnter * setlocal cursorcolumn
  "au WinLeave * setlocal nocursorcolumn
augroup END
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
set completeopt=menu,menuone,preview
set pumheight=16
set autochdir
augroup HardcoreAutoChdir
  au!
  autocmd BufEnter * silent! lcd %:p:h
  autocmd BufEnter * silent! cd %:p:h
augroup END
set nolist
" always try to make the current window 80 columns
set winwidth=80
set nojoinspaces
set maxmempattern=2000000
set maxmem=2000000
set maxmemtot=2000000
set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" mirror tpope dotfiles
"set notimeout
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
augroup MySetTimeoutLen1
  au!
  autocmd InsertEnter * set timeoutlen=100
  autocmd InsertLeave * set timeoutlen=500
augroup END
augroup MySetTimeoutLen2
  au!
augroup END
"set ttimeout
" needed to avoid leaving insert mode delay for vim-airline
set ttimeoutlen=50

" }}}
" Automatic Commands {{{
augroup SaveAllBuffersWhenLosingFocus
  au!
  au FocusLost * silent! wall
augroup END

if !has("gui_running")
  augroup HackToForceAutoreadToWorkCorrectlyInConsoleVim
    au!
    au FocusGained,BufEnter * :silent! !
  augroup END
endif

" Make sure Vim returns to the same line when you reopen a file.
" Thanks, Amit
augroup ReturnToSameLineWhenReopeningFile
  au!
  au BufReadPost *
      \ if line("'\"") > 0 && line("'\"") <= line("$") |
      \     execute 'normal! g`"zv' |
      \     call CenterCursorAesthetically() |
      \ endif
  au BufReadPost COMMIT_EDITMSG
      \ exe 'normal! gg'
augroup END

function! SaveAndCheckIfModified()
  if &modified && !&readonly && len(bufname('%')) > 0
    update
    " too distracting (flickering) and slow
    "SyntasticCheck
  endif
endfunction
augroup SaveWhenExitingInsertMode
  au!
  au InsertLeave * call SaveAndCheckIfModified()
augroup END

function! StripTrailingWhitespace()
    let l:my_saved_winview = winsaveview()
    silent! %s/\s\+$//
    call winrestview(l:my_saved_winview)
endfunction
command! StripTrailingWhitespace call StripTrailingWhitespace()
" generates too many annoying deltas in open source projects like OGRE
"augroup StripTrailingWhitespaceOnSave
    "au!
    "Syandus
    "au BufWritePre C:/SVN/* call StripTrailingWhitespace()

    " C / C++
    "au BufWritePre *.h,*.hpp,*.c,*.cc,*.cpp call StripTrailingWhitespace()
    " Java
    "au BufWritePre *.java call StripTrailingWhitespace()
    " Python
    "au BufWritePre *.py call StripTrailingWhitespace()
    " Lua
    "au BufWritePre *.lua call StripTrailingWhitespace()
"augroup END
"augroup SaveAndRestoreFolds
    "au!
    "au BufWinLeave * silent! mkview
    "au BufWinEnter * silent! loadview
"augroup END
"augroup LocationListAutoOpenClose
    "au!
    " Automatically open, but do not go to (if there are errors) the quickfix /
    " location list window, or close it when is has become empty.
    "
    " Note: Must allow nesting of autocmds to enable any customizations for quickfix
    " buffers.
    " Note: Normally, :cwindow jumps to the quickfix window if the command opens it
    " (but not if it's already open). However, as part of the autocmd, this doesn't
    " seem to happen.
    "autocmd QuickFixCmdPost [^l]* nested cwindow
    "autocmd QuickFixCmdPost    l* nested lwindow
"augroup END
"augroup AlwaysOpenHelpInTheSameWindow
    "autocmd FileType help :wincmd H
"augroup END

"function! AdjustWindowHeight(minheight, maxheight)
  "exe max([min([line("$"), a:maxheight]), a:minheight]) . "wincmd _"
"endfunction
"augroup QuickFixAutoSizer
    "au!
    "au FileType qf call AdjustWindowHeight(3, 16)
"augroup END
" }}}
" wildmenu completion {{{
"set wildmode=longest,list
set wildmode=list:longest
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
set wildignore+=.hg,.git,.svn

" Visual Studio files
set wildignore+=*.ncb,*.suo,*.user,*.vcproj,*.vcxproj,*.out,*.sln,*.pdb
set wildignore+=*.manifest,*.dep,*.idb,*.ipch,*.o,*.obj
set wildignore+=Debug,Release

" GCC
set wildignore+=*.d,*.a

" Gamebryo Binaries
set wildignore+=*.nif,*.kf,*.kfm,*.NSB

" compiled cached bytecodes
set wildignore+=*.pyc,*.luac,*.luc,*.class

" binary document formats
set wildignore+=*.pdf,*.doc,*.docx,*.xls,*.xlsx

" Mac OS X metadata files
set wildignore+=.DS_Store

" Windows OS metadata files
set wildignore+=*.lnk

" Syandus Files
set wildignore+=*.ID
set wildignore+=*.ccv,*.fls,*.pat,*.gsl,*.flt,*.asi

" OGRE
set wildignore+=*.mesh

" Android Files
set wildignore+=*.apk,*.ap_

" }}}
" Tabs, indents, spaces, wrapping {{{
function! SetMyCino()
    set cinoptions=
    set cinoptions+=:0
    set cinoptions+=g0
    set cinoptions+=N-s
    set cinoptions+=(0
    set cinoptions+=u0
    set cinoptions+=Ws
    set cinoptions+=l1
    set cinoptions+=j1
    set cinoptions+=J1
endfunction
call SetMyCino()

set expandtab
set tabstop=2
set shiftwidth=2
set softtabstop=2
" 3 shiftwidths is a little excessive
let g:vim_indent_cont=4

set textwidth=0           " no automatic text wrapping
set formatoptions=qn1
function! ApplyMyFormatOptions()
    set fo=
    set fo+=t   " auto-wrap text using textwidth
    set fo+=c   " auto-wrap comments using textwidth, insert comment leader
    set fo+=q   " allow formatting comments with 'gq'
    set fo+=l   " long lines are not broken in insert mode
    if v:version > 702 || (v:version == 702 && has('patch541'))
        set fo+=j   " remove comment leader when joining lines.
    endif
endfunction
call ApplyMyFormatOptions()
set wrap
set wrapscan
if exists("+breakindent")
    set breakindent
    set breakindentopt=min:20,shift:2,sbr
    set showbreak=>>
else
    exe 'set showbreak=>>\ '
endif
set noswapfile  " computers are pretty reliable nowadays
" }}}

" Leader
let mapleader = ' '
let maplocalleader = ','

" Mouse & selection Behavior
behave xterm                " of course xterm is better
set selectmode=""           " never want SELECT mode
set mousemodel=popup
set keymodel=""
set selection=inclusive
set mousehide
set nomousefocus
set mouse=a
set clipboard=autoselect

" source all other files in the vimfiles/config directory
runtime! config/**/*.vim

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" temporary / quick and dirty mappings for doing random things
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"nmap <leader>1 HWs@param <ESC>elxxj
"nmap <leader>2 HWs@r<ESC>exj
"xmap <leader>3 :s@// @/// @<CR>
"nnoremap <leader>4 :normal ciwSetPaintBoxOutlineZOrder<ESC>
"nnoremap <leader>5 :normal ciwSetPaintZOrder<ESC>
"nnoremap <leader>5 :s/if(/if (/e<CR>:s/( /(/e<CR>:s/ )/)/e<CR>:nohlsearch<CR>
"nnoremap <leader>8 ggVGD
"nnoremap <leader>9 ggVGY

" various snippets and utility functions go here
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

"function! ReplaceWithTLINK()
    "%s/\vvoice\="Audrey16" type\="STREAMING" category\="VOICE"/TLINK=">sounds>amy"/
    "%s/\vvoice\="Mike16" type\="STREAMING" category\="VOICE"/TLINK=">sounds>bob"/
"endfunction
"command! ReplaceWithTLINK silent! call ReplaceWithTLINK()

"map <leader>S VVS<

" vim:fdm=marker:foldlevel=0
