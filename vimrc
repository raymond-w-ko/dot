" when re-sourcing with this set, syntax highlighting changes!
"set nocompatible

" don't customize anything if we are running in evim mode
if v:progname =~? "evim"
    finish
endif

set encoding=utf-8

if exists('+regexpengine')
    " automatic engine selection
    set regexpengine=0
    " use old engine
    "set regexpengine=1
    " use new engine
    "set regexpengine=2
end

let g:omegacomplete_version_preference = 1

" pathogen
let g:pathogen_disabled = []
call add(g:pathogen_disabled, "ack.vim")
call add(g:pathogen_disabled, "cocoa")
call add(g:pathogen_disabled, "YankRing")
call add(g:pathogen_disabled, "vim-easymotion")
call add(g:pathogen_disabled, "powerline")
call add(g:pathogen_disabled, "vim-fireplace")
if g:omegacomplete_version_preference == 2
    if has('java')
        call add(g:pathogen_disabled, "omegacomplete")
    else
        call add(g:pathogen_disabled, "omegacomplete2")
    endif
elseif g:omegacomplete_version_preference == 1
    call add(g:pathogen_disabled, "omegacomplete2")
endif

" check to see if we can use the new powerline
let g:powerline_debugging_pyeval=1
let s:use_new_powerline = 0
if has('python')
    py << EOF
import sys
import vim
if sys.version_info >= (2, 7):
    vim.command('let s:use_new_powerline = 1')
EOF
endif

if s:use_new_powerline
    call add(g:pathogen_disabled, "vim-powerline")
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

if s:use_new_powerline
    if has('win32')
        set rtp+=~/vimfiles/bundle/powerline/powerline/bindings/vim
    else
        set rtp+=~/.vim/bundle/powerline/powerline/bindings/vim
    endif
endif

runtime! plugin/sensible.vim

let s:dir = has('win32') ? '$APPDATA/Vim' : match(system('uname'), "Darwin") > -1 ? '~/Library/Vim' : empty($XDG_DATA_HOME) ? '~/.local/share/vim' : '$XDG_DATA_HOME/vim'
if isdirectory(expand(s:dir))
  if &directory =~# '^\.,'
    let &directory = expand(s:dir) . '/swap//,' . &directory
  endif
  if &backupdir =~# '^\.,'
    let &backupdir = expand(s:dir) . '/backup//,' . &backupdir
  endif
  if exists('+undodir') && &undodir =~# '^\.\%(,\|$\)'
    let &undodir = expand(s:dir) . '/undo//,' . &undodir
  endif
endif
if exists('+undofile')
  set undofile
endif

set fileformats=unix,dos,mac

if has("win32")
    " set this so that RUBY omnicompletion works
    "let g:ruby_path='C:/Ruby192/bin'
else
    if !has("gui_running")
        " need this otherwise colors disappear
        if !exists('g:has_set_my_console_vim_settings')
            set term=xterm
            set t_Co=256
            " Prevent Vim from clobbering the scrollback buffer. See
            " http://www.shallowsky.com/linux/noaltscreen.html
            set t_ti= t_te=

            set t_RV=
            set ttymouse=
            "set ttymouse=xterm2
            let g:has_set_my_console_vim_settings = 1
        endif

        nmap <ESC>t <A-t>
        nmap <ESC>w <A-w>
        nmap <ESC>1 <A-1>
        nmap <ESC>2 <A-2>
        nmap <ESC>3 <A-3>
        nmap <ESC>4 <A-4>
        nmap <ESC>5 <A-5>
    endif

    let s:uname = "win32"
    if has("unix")
        let s:uname = system("uname")
    endif

    if (s:uname == "Darwin\n")
    endif
endif

" General {{{
set autowrite
set autowriteall
set shortmess+=aI    " no intro message
set showmode
set hidden
set novisualbell
set noerrorbells
" WTF, setting visual bell means NO visual bell and NO audio bell in MacVIM
if has('mac')
    set vb
endif
set nocursorline
set nocursorcolumn
set nonumber
if exists('+relativenumber')
    set norelativenumber
endif
set history=1024
set lazyredraw
set ttyfast
set matchtime=0
set splitbelow
set splitright
set notitle
set showtabline=2
set cmdheight=1
set completeopt=menu,menuone,preview
set pumheight=16
set autochdir
set nolist
" always try to make the current window 80 columns
set winwidth=79
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
    "au BufWritePre C:/SVN/* call StripTrailingWhitespace()

    " C / C++
    "au BufWritePre *.h,*.hpp,*.c,*.cc,*.cpp call StripTrailingWhitespace()
    " Java
    "au BufWritePre *.java call StripTrailingWhitespace()
    " Python
    "au BufWritePre *.py call StripTrailingWhitespace()
    " Lua
    "au BufWritePre *.lua call StripTrailingWhitespace()
augroup END
"augroup SaveAndRestoreFolds
    "au!
    "au BufWinLeave * silent! mkview
    "au BufWinEnter * silent! loadview
"augroup END
augroup LocationListAutoOpenClose
    au!
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
augroup END
augroup AlwaysOpenHelpInTheSameWindow
    "autocmd FileType help :wincmd H
augroup END

function! AdjustWindowHeight(minheight, maxheight)
  exe max([min([line("$"), a:maxheight]), a:minheight]) . "wincmd _"
endfunction
augroup QuickFixAutoSizer
    au!
    au FileType qf call AdjustWindowHeight(3, 16)
augroup END
" }}}
" wildmenu completion {{{
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

" OGRE
set wildignore+=*.mesh
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
set nowrap
set wrapscan
if exists("&breakindent")
    set breakindent showbreak=....
elseif has("gui_running")
    set showbreak=\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ ...
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

" vim:fdm=marker:foldlevel=0
