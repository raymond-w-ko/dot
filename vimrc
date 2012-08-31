" when re-sourcing with this set, syntax highlighting changes!
"set nocompatible

" for profiling omegacomplete
"profile start C:/Users/root/Desktop/oc_profile.txt
"profile! file C:/Users/root/vimfiles/bundle/omegacomplete/plugin/omegacomplete.vim

" don't customize anything if we are running in evim mode
if v:progname =~? "evim"
    finish
endif

" paths so that the VIM Ruby interpreter can find its files
if has("win32")
    let g:ruby_path='C:/Ruby192/bin'
    set mouse=a
else
    if !has("gui_running")
        set t_Co=256
        set mouse=
    endif
    for i in range(65,90) + range(97,122)
        let c = nr2char(i)
        exec "map \e" . c . " <M-" . c . ">"
        exec "map! \e" . c . " <M-" . c . ">"
    endfor
endif

" pathogen {{{
let g:pathogen_disabled = []
call add(g:pathogen_disabled, "ack.vim")
call add(g:pathogen_disabled, "autocomplpop")
call add(g:pathogen_disabled, "camelcasemotion")
call add(g:pathogen_disabled, "cocoa")
call add(g:pathogen_disabled, "command-t")
call add(g:pathogen_disabled, "l9")
call add(g:pathogen_disabled, "tagbar")
call add(g:pathogen_disabled, "yankring")
runtime bundle/vim-pathogen/autoload/pathogen.vim
call pathogen#infect()
call pathogen#helptags()
" }}}

" File Options {{{
filetype on             " detect and set filetype
filetype plugin on      " load filetype plugin
filetype indent off     " as a control freak, don't enable automatic indenting
" without a guard, re-sourcing this file breaks vim-easymotion
" re-sourcing also breaks vim-powerline
if !exists("g:already_syntax_on")
    syntax on
    let g:already_syntax_on=1
endif
set fileformats=unix,dos,mac        " order of support
" unfortunately shellslash breaks netrw
set shellslash
" }}}

" General {{{
set encoding=utf-8
set shortmess+=aI    " no intro message
set showmode
set showcmd
set hidden
set novisualbell
set noerrorbells
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
set notimeout
set nottimeout
set autoread
set autowriteall
set title
set showtabline=2
set cmdheight=2
set complete=.,w,b,u,t
set completeopt=menu,menuone,preview
set pumheight=16
set autochdir
set nolist
set listchars=tab:▸\ ,eol:¬
set winwidth=80
" Prevent Vim from clobbering the scrollback buffer. See
" http://www.shallowsky.com/linux/noaltscreen.html
"set t_ti= t_te=
"set noesckeys
set maxmempattern=4096
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
    "au BufWritePre C:/SVN/* call StripTrailingWhitespace()
    "au BufWritePre *.h,*.hpp,*.c,*.cc,*.cpp call StripTrailingWhitespace()
    "au BufWritePre *.py call StripTrailingWhitespace()
augroup END
augroup SaveAndRestoreFolds
    au!
    "au BufWinLeave * silent! mkview
    "au BufWinEnter * silent! loadview
augroup END
" }}}
" wildmenu completion {{{
set wildmenu
set wildmode=longest,list
set wildchar=<Tab>

" binaries with a 99.9% chance of not being edited
set wildignore+=*.exe,*.dll

" media files in a binary format
set wildignore+=*.jpg,*.jpeg,*.png,*.gif,*.bmp,*.tga,*.mp3,*.ico,*.wav
set wildignore+=*.bik,*.ani,*.mask

" version control directories
set wildignore+=.hg,.git,.svn

" Visual Studio files
set wildignore+=*.ncb,*.suo,*.user,*.vcproj,*.vcxproj,*.out,*.sln,*.pdb,*.manifest,*.dep,*.idb,*.ipch,*.o,*.obj
set wildignore+=Debug,Release

" Gamebryo Binaries
set wildignore+=*.nif,*.kf,*.kfm,*.NSB

" compiled cached bytecodes
set wildignore+=*.pyc,*.luac

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
set nocindent
set nosmartindent

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
let mapleader = ","
let maplocalleader = "\\"

" Mouse & selection Behavior
behave xterm                " of course xterm is better
set selectmode=""           " never want SELECT mode
set mousemodel=popup
set keymodel=""
set selection=inclusive
set mousehide
set nomousefocus

" source all other files in the vimfiles/config directory
runtime! config/**/*.vim

set previewheight=1

function! ResizeFixer()
    if &previewwindow
        resize 1
        return
    endif

    if bufname("%") == '__Scratch__'
        resize 1
        return
    endif
endfunction
"augroup ScratchWindowResizer
    "au!
    "au WinEnter * call ResizeFixer()
"augroup END

nmap <F2> :e C:\SVN\Syandus_ALIVE4\Platform\Source\Code\SyCore\FakeGamebryoStubs.cpp<CR>

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

" vim:fdm=marker:foldlevel=0
