" vim:fdm=marker:foldlevel=0

" when re-sourcing with this set, syntax highlighting changes!
"set nocompatible

" don't customize anything if we are running in evim mode
if v:progname =~? "evim"
    finish
endif

if has("win32")
    " set this so that RUBY omnicompletion works
    "let g:ruby_path='C:/Ruby192/bin'
else
    if !has("gui_running")
        set t_Co=256
        " Prevent Vim from clobbering the scrollback buffer. See
        " http://www.shallowsky.com/linux/noaltscreen.html
        set t_ti= t_te=
        set ttymouse=xterm2

        nmap <ESC>t <A-t>
        nmap <ESC>w <A-w>
        nmap <ESC>1 <A-1>
        nmap <ESC>2 <A-2>
        nmap <ESC>3 <A-3>
        nmap <ESC>4 <A-4>
        nmap <ESC>5 <A-5>
    endif
endif

" pathogen {{{
let g:pathogen_disabled = []
call add(g:pathogen_disabled, "ack.vim")
call add(g:pathogen_disabled, "cocoa")
call add(g:pathogen_disabled, "YankRing")
call add(g:pathogen_disabled, "vim-easymotion")
call add(g:pathogen_disabled, "VimClojure")
runtime bundle/vim-pathogen/autoload/pathogen.vim
call pathogen#infect()
call pathogen#helptags()
" }}}

" File Options {{{
filetype on             " detect and set filetype
filetype plugin on      " load filetype plugin
filetype indent on      " as a control freak, don't enable automatic indenting
" without a guard, re-sourcing this file breaks vim-easymotion
" re-sourcing also breaks vim-powerline
if !exists("g:already_syntax_on")
    syntax on
    let g:already_syntax_on=1
endif
set fileformats=unix,dos,mac        " order of support
" unfortunately shellslash breaks netrw, but I don't really use it
"set shellslash
" }}}

" General {{{
set encoding=utf-8
set shortmess+=aI    " no intro message
set showmode
set showcmd
set hidden
set novisualbell
set noerrorbells
" WTF, setting visual bell means NO visual bell and NO audio bell in MacVIM
if has('mac')
    set vb
endif
set nocursorcolumn
set ruler
set backspace=indent,eol,start
set nonumber
if exists('+relativenumber')
    set norelativenumber
endif
set laststatus=2
set history=1024
set lazyredraw
set ttyfast
set showmatch
set matchtime=0
set splitbelow
set splitright
set noesckeys
set notimeout
set nottimeout
set autoread
set autowriteall
set notitle
set showtabline=2
set cmdheight=2
set complete=.,w,b,u,t
set completeopt=menu,menuone,preview
set pumheight=16
set autochdir
set nolist
set listchars=tab:▸\ ,eol:¬
" always try to make the current window 80 columns
set winwidth=80
set maxmempattern=2000000
set maxmemtot=2000000
" }}}
" Automatic Commands {{{
augroup SaveAllBuffersWhenLosingFocus
    au!
    au FocusLost * silent! wall
augroup END

" Make sure Vim returns to the same line when you reopen a file.
" Thanks, Amit
augroup ReturnToSameLineWhenReopeningFile
    au!
    au BufReadPost *
        \ if line("'\"") > 0 && line("'\"") <= line("$") |
        \     execute 'normal! g`"zv' |
        \     call AestheticCenterCursor() |
        \ endif
    au BufReadPost COMMIT_EDITMSG
        \ exe 'normal! gg'
augroup END

function! StripTrailingWhitespace()
    let l:my_saved_winview = winsaveview()
    silent! %s/\s\+$//
    call winrestview(l:my_saved_winview)
endfunction
augroup StripTrailingWhitespaceOnSave
    au!
    "Syandus
    au BufWritePre C:/SVN/* call StripTrailingWhitespace()

    " C / C++
    au BufWritePre *.h,*.hpp,*.c,*.cc,*.cpp call StripTrailingWhitespace()
    " Java
    au BufWritePre *.java call StripTrailingWhitespace()
    " Python
    au BufWritePre *.py call StripTrailingWhitespace()
    " Lua
    au BufWritePre *.lua call StripTrailingWhitespace()
augroup END
"augroup SaveAndRestoreFolds
    "au!
    "au BufWinLeave * silent! mkview
    "au BufWinEnter * silent! loadview
"augroup END
" }}}
" wildmenu completion {{{
set wildmenu
set wildmode=longest,list
set wildchar=<Tab>

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
" }}}
" Tabs, indents, spaces, wrapping {{{
set autoindent
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
set tabstop=4
set shiftwidth=4
set softtabstop=4
set shiftround
set nosmarttab
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
if exists("&breakindent")
    set breakindent showbreak=....
elseif has("gui_running")
    set showbreak=\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ ...
endif
" }}}
" swap, undo, backup {{{
set directory=~/vimtmp//
set backupdir=~/vimbackup//

set viewdir=~/vimview//
set viewoptions=cursor,folds,slash,unix

if exists('+undofile')
    set undofile
    set undodir=~/vimundo//
    set undolevels=8192     " maximum number of changes that can be undone
    set undoreload=65535    " maximum number lines to save for undo on a buffer reload
endif

set backup              " might as well, it doesn't really hurt

set noswapfile          " computers are pretty reliable nowadays
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

nmap <leader>1 HWs@param <ESC>elxxj
nmap <leader>2 HWs@r<ESC>exj
xmap <leader>3 :s@// @/// @<CR>
nnoremap <leader>4 :normal ciwSetPaintBoxOutlineZOrder<ESC>
nnoremap <leader>5 :normal ciwSetPaintZOrder<ESC>
nnoremap <leader>5 :s/if(/if (/e<CR>:s/( /(/e<CR>:s/ )/)/e<CR>:nohlsearch<CR>

nnoremap <leader>8 ggVGD
nnoremap <leader>9 ggVGY
function! MakeSyDllFuncsH()
endfunction
nnoremap <leader>0 :call MakeSyDllFuncsH()<ESC>

function! FilterSmartQuotes()
    %s/\v‘|’/\'/
endfunction
command! FilterSmartQuotes silent! call FilterSmartQuotes()

if has('java')
    let jar_list = [
                \ 'C:/cygwin/home/root/src/vim/src/java/vim.jar',
                \ expand("$HOME") . "/java/clojure-1.5.0-RC1.jar",
                \ expand("$HOME") . "/java/groovy-all-2.0.6-indy.jar"
                \ ]
    let jars = substitute(join(jar_list, ';'), '\\', '/', 'g')
    exe "set javacp=" . jars

    javashell clojure
endif
