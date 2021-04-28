" vim:fdm=marker

" http://utf8everywhere.org/
set encoding=utf-8

" speed hacks
let g:loaded_rrhelper=1
let g:did_install_default_menus=1 " avoid stupid menu.vim (saves ~100ms)

let s:use_treesitter=has("nvim")

augroup MyVimrc
  au!
augroup END

if filereadable(expand("$HOME/.has-full-github-access"))
  let g:plug_url_format = 'git@github.com:%s.git'
  let g:plug_shallow = 0
endif

" if this is not set early enough, causes last row highlight in neovim
if exists("g:started_by_firenvim")
  set cmdheight=1
else
  set cmdheight=2
endif
" Leader
let mapleader = "\<Space>"
let maplocalleader = ","

let s:cywgin_vim_dir = expand("C:/cygwin64/home/$USERNAME/dot/.vim/plugged")
if isdirectory(s:cywgin_vim_dir)
  call plug#begin(s:cywgin_vim_dir)
else
  call plug#begin('~/.vim/plugged')
endif

" manually managed plugins
Plug '$HOME/dot/.vim/plugged.manual/lightline-colorschemes'
Plug '$HOME/dot/.vim/plugged.manual/rko-misc'
nnoremap <leader>o :ToggleWord<CR>

if has("nvim")
  Plug 'glacambre/firenvim'
endif

" my plugins
Plug 'raymond-w-ko/vim-solarized8'
" Plug 'raymond-w-ko/scrollfix'
let g:scrollfix=50
Plug 'raymond-w-ko/vim-eslisp'
Plug 'raymond-w-ko/vim-lua-indent'

Plug 'raymond-w-ko/vim-geckocomplete'
inoremap <silent><expr><nowait> <C-s> geckocomplete#toggle_pause_completion()
inoremap <silent><expr><nowait> <Tab> geckocomplete#completion_key()

" colorscheme
Plug 'lifepillar/vim-colortemplate'

" finders
" Plug 'ctrlpvim/ctrlp.vim'
" Plug 'FelikZ/ctrlp-py-matcher'
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'

" potpourri
Plug 'vim-jp/vital.vim'
Plug 'kana/vim-operator-user'
Plug 'qpkorr/vim-bufkill'
let g:BufKillCreateMappings = 0
Plug 'Konfekt/FastFold'
Plug 'itchyny/lightline.vim'
if has('python3') || has('python')
  Plug 'SirVer/ultisnips'
endif
Plug 'honza/vim-snippets'
Plug 'dense-analysis/ale'
Plug 'maximbaz/lightline-ale'
Plug 'sjl/gundo.vim'
let g:gundo_right=1
Plug 'mbbill/undotree'
Plug 'zhimsel/vim-stay'
Plug 'godlygeek/tabular'
Plug 'mattn/emmet-vim'

if !s:use_treesitter
  Plug 'luochen1990/rainbow'
  augroup MyRaindowLoader
    auto FileType * call rainbow_main#load()
    auto colorscheme * call rainbow_main#load()
  augroup end
endif

Plug 'chrisbra/Colorizer'
Plug 'majutsushi/tagbar'
Plug 'tommcdo/vim-lion'
Plug 'tommcdo/vim-exchange'
Plug 'AndrewRadev/linediff.vim'
let g:tagbar_sort=0 " usually my tags are in some sort of custom order that makes sense
let g:tagbar_width=40

if !exists("g:started_by_firenvim")
  Plug 'mhinz/vim-startify'
endif
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

" this plugin is now obsolete and no longer needed as both neovim and vim
" (since version 8.2.2345) have native support for this functionality.
" Plug 'tmux-plugins/vim-tmux-focus-events'
Plug 'christoomey/vim-tmux-navigator'
" obsoleted by vim-tmux-navigator
" nmap <C-h> <C-w>h
" nmap <C-j> <C-w>j
" nmap <C-k> <C-w>k
" nmap <C-l> <C-w>l
let g:tmux_navigator_no_mappings = 1
nnoremap <silent> <C-h> :TmuxNavigateLeft<cr>
nnoremap <silent> <C-j> :TmuxNavigateDown<cr>
nnoremap <silent> <C-k> :TmuxNavigateUp<cr>
nnoremap <silent> <C-l> :TmuxNavigateRight<cr>

nnoremap <silent> <Left> :TmuxNavigateLeft<cr>
nnoremap <silent> <Right> :TmuxNavigateRight<cr>
nnoremap <silent> <Up> :TmuxNavigateUp<cr>
nnoremap <silent> <Down> :TmuxNavigateDown<cr>

" Tim Pope
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-obsession'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-characterize'
Plug 'tpope/vim-rsi'
Plug 'tpope/vim-tbone'
Plug 'tpope/vim-fugitive'
nnoremap <leader>gs :Git<CR>
nnoremap <leader>gw :Git write<CR>
nnoremap <leader>gc :Git commit --verbose<CR>
nnoremap <leader>gp :Git push<CR>
nnoremap <leader>G :Git<Space>
Plug 'tpope/vim-rhubarb'
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
Plug 'kana/vim-arpeggio'
Plug 'raymond-w-ko/vim-asterisk'
let g:asterisk#keeppos=1
Plug 'osyo-manga/vim-anzu'
map n <Plug>(anzu-n-with-echo)
map N <Plug>(anzu-N-with-echo)
map *  <Plug>(asterisk-z*)
map g* <Plug>(asterisk-gz*)
map #  <Plug>(asterisk-z#)
map g# <Plug>(asterisk-gz#)
map / /\v
map ? ?\v

" Junegunn Choi
Plug 'junegunn/vim-easy-align'
Plug 'junegunn/vim-peekaboo'
let g:peekaboo_window="vert bo 80new"
Plug 'junegunn/gv.vim'

" shougo + friends (Japanese Vim)
Plug 'roxma/nvim-yarp'
Plug 'roxma/vim-hug-neovim-rpc'
Plug 'roxma/vim-tmux-clipboard'

""""""""""""""""""""""""""""""""""""""""
" Justin M. Keyes
""""""""""""""""""""""""""""""""""""""""
Plug 'justinmk/vim-dirvish'
" Disable netrw, but autoload it for `gx`.
let g:dirvish_mode = 1
let g:dirvish_relative_paths = 1
augroup dirvish_config
  autocmd!
  " Map `t` to open in new tab.
  autocmd FileType dirvish
    \  nnoremap <silent><buffer> t :call dirvish#open('tabedit', 0)<CR>
    \ |xnoremap <silent><buffer> t :call dirvish#open('tabedit', 0)<CR>
  " Map `gr` to reload.
  autocmd FileType dirvish nnoremap <silent><buffer> gr :<C-U>Dirvish %<CR>
  " Map `gh` to hide dot-prefixed files.  Press `R` to "toggle" (reload).
  autocmd FileType dirvish nnoremap <silent><buffer> gh
      \ :silent keeppatterns g@\v/\.[^\/]+/?$@d _<cr>:setl cole=3<cr>
augroup END
let g:loaded_netrwPlugin = 0
nmap gx <Plug>NetrwBrowseX
nnoremap <Plug>NetrwBrowseX :call netrw#BrowseX(expand((exists("g:netrw_gx")? g:netrw_gx : '<cfile>')),netrw#CheckIfRemote())<CR>

Plug 'justinmk/vim-sneak'
let g:sneak#label=1
let g:sneak#s_next=1
let g:sneak#use_ic_scs=1
highlight Sneak guifg=magenta guibg=black ctermfg=black ctermbg=red
highlight SneakScope guifg=black guibg=#00ff00 ctermfg=black ctermbg=green
highlight SneakLabel guifg=magenta guibg=black ctermfg=black ctermbg=red
highlight SneakLabelMask guifg=black guibg=black ctermfg=black ctermbg=black

Plug 'justinmk/vim-gtfo'
Plug 'justinmk/vim-printf'

" LucHermitte
Plug 'LucHermitte/lh-vim-lib'
Plug 'LucHermitte/alternate-lite'

" Web Development
Plug '2072/PHP-Indenting-for-VIm'
Plug 'pangloss/vim-javascript'
Plug 'hail2u/vim-css3-syntax'
Plug 'othree/csscomplete.vim'
augroup MyVimrc
  autocmd FileType css set omnifunc=csscomplete#CompleteCSS noci
augroup END
Plug 'groenewege/vim-less'

Plug 'jpalardy/vim-slime'
let g:slime_no_mappings = 1
let g:slime_target="tmux"
let g:slime_default_config = {"socket_name": "default", "target_pane": "1"}

Plug 'reasonml-editor/vim-reason-plus'

" lisp
Plug 'clojure-vim/clojure.vim'
Plug 'guns/vim-sexp'
let g:sexp_filetypes = 'clojure,scheme,lisp,timl,eslisp'
let g:sexp_insert_after_wrap = 1
let g:sexp_enable_insert_mode_mappings = 1
Plug 'tpope/vim-sexp-mappings-for-regular-people'
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
let g:clj_fmt_autosave = 0
augroup MyVimrc
  au!
  " au FileType clojure nnoremap <buffer> <leader>r :Require<CR>
  " au FileType clojure nnoremap <buffer> <leader>R :Require!<CR>
  au FileType clojure nnoremap <buffer> <leader>f :Cljfmt<CR>
augroup END

" misc filetypes
Plug 'octol/vim-cpp-enhanced-highlight'
Plug 'aklt/plantuml-syntax'
Plug 'rhysd/vim-clang-format'
let g:clang_format#code_style="google"
let g:clang_format#detect_style_file=1
augroup MyVimrc
  autocmd FileType c,cpp,objc nnoremap <buffer><Leader>f :<C-u>ClangFormat<CR>zz
  autocmd FileType c,cpp,objc vnoremap <buffer><Leader>f :ClangFormat<CR>
augroup END

if has("nvim") && s:use_treesitter
  " :TSUpdate
  " Plug 'nvim-treesitter/nvim-treesitter'
  Plug 'raymond-w-ko/nvim-treesitter'
  Plug 'p00f/nvim-ts-rainbow'
  Plug 'nvim-treesitter/playground'
endif

call plug#end()

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" General {{{

" load sensible defaults by our prophet Tim Pope
runtime! plugin/sensible.vim
set scrolloff=0 " scrolloff 0 is needed by scrollfix
" set scrolloff=9001 " scrolloff 0 is needed by scrollfix
set undofile
set backup
set writebackup
set backupcopy=auto
set noswapfile  " computers are pretty reliable nowadays

" vim
if !has("nvim")
  set directory=$HOME/.local/share/vim/swap//
  set backupdir=$HOME/.local/share/vim/backup
  set undodir=$HOME/.local/share/vim/undo
endif
if !isdirectory(&directory)
  echoerr "'directory' does not exists: " . &directory
endif
set backupdir-=.
if !isdirectory(&backupdir)
  echoerr "'backupdir' does not exists: " . &backupdir
endif
if !isdirectory(&undodir)
  echoerr "'undodir' does not exists: " . &undodir
endif

if !exists("g:rko_already_setup_syntax")
  if s:use_treesitter
    syntax enable
  else
    syntax enable
  endif
  syntax conceal on
  let g:rko_already_setup_syntax=1
endif
set fileformats=unix,dos
set autowrite
set autowriteall
set updatetime=2000
set shortmess+=aIcF
set report=0 " report back when greater than N lines changed
set showmode
set hidden
set novisualbell
set noerrorbells
if exists('+belloff')
  set belloff=all
endif
set ruler
set number
if has("nvim") || has("patch-8.1.1564")
  " Recently vim can merge signcolumn and number column into one
  set signcolumn=number
else
  set signcolumn=yes
endif
if exists('+relativenumber')
  set norelativenumber
endif
" setting this to 10000 actually causes noticable exit lag
set history=128
set lazyredraw
set showcmd
set ttyfast
set matchtime=0
set splitbelow
set splitright
set notitle
if exists("g:started_by_firenvim")
  set showtabline=0
  set laststatus=0
else
  set showtabline=2
endif
set completeopt+=menu
set completeopt+=menuone
set completeopt+=preview
set pumheight=16
" this breaks dirvish
" set autochdir
" always try to make the current window 80 columns
set winwidth=80
set nojoinspaces
set maxmempattern=2000000
if !has("nvim")
  set maxmem=2000000
  set maxmemtot=2000000
endif
set nolist
set listchars=tab:\|\ ,trail:•,extends:>,precedes:<,nbsp:+
" Mouse & selection Behavior
behave xterm                " of course xterm is better
set selectmode=""           " never want SELECT mode
set mousemodel=popup
set keymodel=""
set selection=inclusive
set mousehide
set nomousefocus
set mouse=a
if !has("nvim")
  if has("mouse_sgr")
    set ttymouse=sgr
  else
    set ttymouse=xterm2
  end
  set clipboard=autoselect
endif
set pastetoggle=<F9>

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
set sessionoptions-=winpos
set sessionoptions-=help
set sessionoptions-=globals
set sessionoptions-=blank

set viewoptions=cursor,folds,slash,unix

set cinoptions=
set cinoptions+=:0
set cinoptions+=g0
set cinoptions+=N-s
" this by itself breaks vim-synesthesia + vim-niji parent parsing
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
  let default_home = expand('$HOME')
  let possible_homes = [
      \ 'C:/cygwin/home/rko',
      \ 'C:/cygwin64/home/rko',
      \ 'C:/cygwin/home/Raymond W. Ko',
      \ 'C:/cygwin64/home/Raymond W. Ko',
      \ 'C:/cygwin/home/root',
      \ 'C:/cygwin64/home/root',
      \ default_home,
      \ ]
  for dir in homes
    if isdirectory(dir)
      let s:unix_home = dir
      break
    endif
  endfor
elseif has('win32unix')
  let s:unix_home = expand('$HOME')
else
  let s:unix_home = expand('$HOME')
endif

" traverse up parent directories until it finds one that matches in the above
" list
function! s:IsProjectDirectory(directory)
  if isdirectory(a:directory . "/.git")
    return 1
  elseif isdirectory(a:directory . "/.hg")
    return 1
  elseif filereadable(a:directory . "/shadow-cljs.edn")
    return 1
  else
    return 0
  endif
endfunction
function! MyGetProjectDirectory()
  let last_directory = ''
  let directory = expand("%:p:h")

  while !s:IsProjectDirectory(directory) && last_directory != directory
    let last_directory = directory
    let directory = substitute(simplify(directory . '/..'),
        \ '[\\/]*$', '', '')
  endwhile

  if last_directory == directory
    " we could not find a project directory
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
command! -bar HexMode call ToggleHex()

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

" }}}
" GUI {{{
if exists('+termguicolors')
  set termguicolors
  if !has("nvim")
    let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
    let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
  endif
endif
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
  let needle = getreg('"')
  " let needle = escape(needle, ">")
  echom needle
  let dir = MyGetProjectDirectory()
  call fzf#run({
      \ "source": "rg --fixed-strings -- '" . needle . "'",
      \ "sink": function("s:rg_handler"),
      \ "options": printf('--color="dark,hl:33,hl+:#ff0000,fg+:235,bg+:#000000,fg+:254,info:254,prompt:37,spinner:108,pointer:235,marker:235" --prompt "%s"', dir),
      \ "dir": dir,
      \ "window": {"width": 0.618, "height": 0.618,},})
endfunction
nnoremap <leader>r :call <SID>FindWordInProject()<CR>

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
" this variant by Petr Zemek allows j and k to work properly when using
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

" }}}

syntax sync fromstart
set foldlevelstart=9001

" Text Objects
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

" inoremap kj <Esc>
let g:arpeggio_timeoutlen=90
call arpeggio#map('i', '', 0, 'jk', '<Esc>')

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
nnoremap <Tab> :tabprev<CR>
nnoremap \ :tabnext<CR>
" call arpeggio#map('n', '', 0, 'er', ':tabprev<CR>')
" call arpeggio#map('n', '', 0, 'ui', ':tabnext<CR>')

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
    inoremap <silent><buffer> <BS> <C-r>=<SID>EmptyPairDeleterBackspace()<CR>
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
    nnoremap <leader>el :e ~/dot/.vim/lua/rko.lua<CR>
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
nmap <leader><leader> <C-^>

" This allows for change paste motion cp{motion}
" http://stackoverflow.com/questions/2471175/vim-replace-word-with-contents-of-paste-buffer
function! ChangePaste(type, ...)
    silent exe "normal! `[v`]\"_c"
    silent exe "normal! p"
endfunction
nnoremap <silent> cp :set opfunc=ChangePaste<CR>g@

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

" }}}

" autocommands {{{

" ----------------------------------------------------------------------------
" Help in new tabs
" ----------------------------------------------------------------------------
function! s:SetupHelpTab()
  if &buftype == 'help'
    " silent wincmd T
    nnoremap <buffer> q :q<cr>
  endif
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
    \ "conf": 1,
    \ "tmux": 1,
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
let s:web_filetypes = {
    \ "css": 1,
    \ }

hi LimeGreen guifg=#00ff00 guibg=#002b36 gui=none ctermbg=0 ctermfg=46 term=none cterm=none
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
  if &filetype == "clojure"
    syntax match rkoClojureMacro /\v<def-[a-zA-Z0-9_-].+>/ containedin=ALL
    syntax match rkoClojureMacro /\v<defn-[a-zA-Z0-9-]+>/ containedin=ALL
    syntax match rkoClojureMacro /\v<deftest>/ containedin=ALL
    syntax match rkoClojureMinorMacro /\v<:let>/ containedin=ALL
    syntax match rkoClojureMinorMacro /\v<:plet>/ containedin=ALL
    syntax match rkoClojureMinorMacro /\v<:pplet>/ containedin=ALL
    syntax match rkoClojureMinorMacro /\v<:do>/ containedin=ALL
    syntax match rkoClojureMinorMacro /\v<:pdo>/ containedin=ALL
    syntax match rkoClojureMinorMacro /\v<:else>/ containedin=ALL
    syntax match rkoClojureMinorMacro /\v<:return>/ containedin=ALL
  endif
  if &filetype == "javascript"
    syntax match Keyword /\v<await>/ containedin=ALL
    syntax match Keyword /\v<async>/ containedin=ALL
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

  syntax match rkoTODO /\v<TODO|TODO:|XXX|XXX:|NOTE|NOTE:|WARN|WARN:>/ containedin=ALL
  syntax match rkoError /\verror/ containedin=ALL

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
  highlight link rkoClojureMacro IncSearch
  highlight link rkoClojureMinorMacro LimeGreen
  highlight link rkoClojureConceal PreProc
  highlight link rkoError Define

  if has_key(s:version_control_filetypes, &filetype)
    syntax region rkoVersionControlDelete start=/\v^-/ end=/\v$/
    syntax region rkoVersionControlAdd start=/\v^\+/ end=/\v$/
  endif
  highlight link rkoVersionControlDelete DiffDelete
  highlight link rkoVersionControlAdd DiffAdd
  highlight link rkoTODO Define

  highlight link gitMergeConflict Error
  syntax match gitMergeConflict /^=======$/ containedin=ALL
  syntax match gitMergeConflict /^<<<<<<< .\+$/ containedin=ALL
  syntax match gitMergeConflict /^>>>>>>> .\+$/ containedin=ALL

  if &filetype == "clojure"
    runtime manual/rko_clojure.vim
    syntax keyword rkoClojureConceal fn conceal cchar=λ containedin=ALL
    setl conceallevel=1
  elseif &filetype == "dirvish"
    runtime syntax/dirvish.vim
  elseif &filetype == "html"
    runtime syntax/html.vim
    runtime after/syntax/html.vim
  elseif &filetype == "css"
    runtime syntax/css.vim
    runtime after/syntax/css.vim
  elseif &filetype == "java"
    runtime syntax/java.vim
  endif
endfunction

func MyShowSyntaxGroups() abort
  call feedkeys("\<Plug>ScripteaseSynnames")
  TSHighlightCapturesUnderCursor
endf
func MyCenterCursor() abort
  normal! zz
endf

augroup MyVimrc
  " only show cursorline if a window has focus
  " this noticably slows down VIM in files with complicated syntax highlighting,
  " like PHP, so disable it for now.
  au VimEnter,WinEnter,BufWinEnter * setlocal cursorline
  au WinLeave * setlocal nocursorline

  "au VimEnter,WinEnter,BufWinEnter * setlocal cursorcolumn
  "au WinLeave * setlocal nocursorcolumn

  " check when cursor stops moving
  " au CursorHold,CursorHoldI * :silent! checktime
  " hack for console VIM so that check for changed files work correctly
  " au FocusGained,BufEnter * :silent! checktime

  " hardcore autochdir
  " autocmd BufEnter * silent! lcd %:p:h
  " autocmd BufEnter * silent! cd %:p:h

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

  if s:use_treesitter
    autocmd FileType * nnoremap <buffer> zS :silent call MyShowSyntaxGroups()<CR>
  endif

  " autocmd FileType * call s:SetupBasicSyntaxHighlights()
  " autocmd BufEnter * :syntax sync fromstart

  " autocmd InsertEnter * setlocal nolist
  " autocmd InsertLeave * setlocal list
  
  autocmd CursorMoved * call MyCenterCursor()
  autocmd BufReadPost *.html hi clear htmlItalic
  autocmd BufReadPost *.html hi clear TSEmphasis
augroup END

" }}}
" Plugins {{{

let g:loaded_matchparen = 1

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" a.vim
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:alternateNoDefaultAlternate=1

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
" vim-clojure-static
" clojure-vim/clojure.vim
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
" default is 300
let g:clojure_maxlines = 256

let g:clojure_fuzzy_indent = 1
let g:clojure_fuzzy_indent_patterns = ['^with', '^def', '^let', '^go-loop$', '^comment$', 'fdef$', '^profile$', '^p$']
let g:clojure_fuzzy_indent_blacklist = ['-fn$', '\v^with-%(meta|out-str|loading-context)$', '^cond-xlet$']
let g:clojure_special_indent_words = 'deftype,defrecord,reify,proxy,extend-type,extend-protocol,letfn,comment'


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ale
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:ale_lint_on_text_changed=0
let g:ale_lint_on_insert_leave=0
let g:ale_lint_on_enter=1
let g:ale_lint_on_save=1
let g:ale_lint_on_filetype_changed=1

let g:ale_sign_column_always=0
let g:ale_set_highlights=0
let g:ale_set_signs=1
let g:ale_set_balloons=0
let g:ale_close_preview_on_insert=1
let g:ale_echo_delay=500

let g:ale_virtualtext_cursor=0
let g:ale_virtualtext_delay=500

let g:ale_floating_window_border=['│', '─', '╭', '╮', '╯', '╰']

if has("nvim")
  let g:ale_hover_to_floating_preview=1
  let g:ale_detail_to_floating_preview=1
  let g:ale_echo_cursor=1
  let g:ale_floating_preview=1
  let g:ale_cursor_detail=1
else
  let g:ale_echo_cursor=1
  let g:ale_cursor_detail=0
endif

let g:ale_linters = {
    \ "jsx": ["eslint"],
    \ "c": [],
    \ "cpp": [],
    \ "clojure": ["joker"],
    \ "python": ["pycodestyle", "pylint"],
    \ }

" ale_linters/sql/sqllint.vim
" Author: Joe Reynolds <joereynolds952@gmail.co>
" Description: sql-lint for SQL files.
"              sql-lint can be found at
"              https://www.npmjs.com/package/sql-lint
"              https://github.com/joereynolds/sql-lint

function! ALE_sqldashlint_Handle(buffer, lines) abort
    " Matches patterns like the following:
    "
    " stdin:1 [ER_NO_DB_ERROR] No database selected
    let l:pattern = '\v^[^:]+:(\d+) (.*)'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \   'lnum': l:match[1] + 0,
        \   'col': l:match[2] + 0,
        \   'type': l:match[3][0],
        \   'text': l:match[0],
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('sql', {
\   'name': 'sqldashlint',
\   'executable': 'sql-lint',
\   'command': 'sql-lint',
\   'callback': 'ALE_sqldashlint_Handle',
\})

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
\    'fzf': 0,
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

" }}}

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
function! MyHtmlFormatter()
  let view = winsaveview()
  execute "%!prettier --parser html"
  call winrestview(view)
endfunction
function! MyScssFormatter()
  let view = winsaveview()
  execute "%!prettier --parser scss"
  call winrestview(view)
endfunction
function! MyCssFormatter()
  let view = winsaveview()
  execute "%!prettier --parser css"
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

" When using `dd` in the quickfix list, remove the item from the quickfix list.
function! RemoveQFItem()
  let curqfidx = line('.') - 1
  let qfall = getqflist()
  call remove(qfall, curqfidx)
  call setqflist(qfall, 'r')
  execute curqfidx + 1 . "cfirst"
  :copen
endfunction
command! RemoveQFItem :call RemoveQFItem()

augroup MyVimrc
  au BufWritePost *.vimrc source $MYVIMRC
  au BufWritePost *.gvimrc source $MYGVIMRC

  " Use map <buffer> to only map dd in the quickfix window. Requires +localmap
  autocmd FileType qf map <buffer> dd :RemoveQFItem<cr>
  au FileType gitcommit setlocal foldlevel=9001

  " au BufNewFile,BufRead *.py setlocal foldmethod=syntax foldlevel=1
  au BufNewFile,BufRead *.py setlocal nofoldenable
  au BufNewFile,BufRead *.py setlocal omnifunc=pythoncomplete#Complete

  au FileType vim setlocal commentstring=\"\ %s
  au FileType cmake setlocal commentstring=#\ %s
  au FileType dosbatch setlocal ff=dos
  au FileType dosbatch setlocal commentstring=REM\ %s
  au FileType Makefile setlocal noexpandtab
  au FileType markdown setlocal et list sw=4 sts=4 ts=4

  au BufReadPost *.hlsl set filetype=fx
  
  au FileType css,less,scss setlocal iskeyword+=-
  au FileType javascript setlocal iskeyword+=$
  au FileType javascript setlocal cinoptions=g0,N-s,(0,u0,Ws,l1,j1,J1
  autocmd FileType json nnoremap <buffer> <Leader>f :call MyJsonFormatter()<CR>
  autocmd FileType javascript nnoremap <buffer> <Leader>f :call MyJavascriptFormatter()<CR>
  autocmd FileType javascript.jsx nnoremap <buffer> <Leader>f :call MyJavascriptFormatter()<CR>
  autocmd FileType scss nnoremap <buffer> <Leader>f :call MyScssFormatter()<CR>
  autocmd FileType css nnoremap <buffer> <Leader>f :call MyCssFormatter()<CR>
  autocmd FileType python nnoremap <buffer> <Leader>f :call MyPythonFormatter()<CR>
  autocmd FileType html nnoremap <buffer> <Leader>f :call MyHtmlFormatter()<CR>
  autocmd FileType go nnoremap <buffer> <Leader>f :call MyGoFormatter()<CR>
  
  au FileType markdown setlocal textwidth=80

  au BufRead .joker set ft=clojure
augroup END

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
if has('unix')
  nnoremap <leader>m :update<CR>:call FindAndRunMakefile()<CR>
endif

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" random stuff
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" converts underscore_case to camelCase
" nnoremap <leader>c :s#_\(\l\)#\u\1#<CR>
" vnoremap <leader>c :s#_\(\l\)#\u\1#<CR>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" neovim lua
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
if has("nvim")
  lua require("rko")
endif

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" firenvim
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:firenvim_config = { 
    \ 'globalSettings': {
        \ 'alt': 'all',
        \ 'ignoreKeys': {
            \ 'all': [],
            \ 'normal': [],
        \ }
    \  },
    \ 'localSettings': {
        \ '.*': {
            \ 'cmdline': 'neovim',
            \ 'content': 'text',
            \ 'priority': 0,
            \ 'selector': 'textarea:not([readonly]), div[role="textbox"]',
            \ 'takeover': 'never',
        \ },
    \ }
\ }
let fc = g:firenvim_config['localSettings']
let fc['https?://hypothetical\.example\.com/'] = { 'takeover': 'never', 'priority': 1 }

function! s:IsFirenvimActive(event) abort
  if !exists('*nvim_get_chan_info')
    return 0
  endif
  let l:ui = nvim_get_chan_info(a:event.chan)
  return has_key(l:ui, 'client') && has_key(l:ui.client, 'name') &&
      \ l:ui.client.name =~? 'Firenvim'
endfunction
fun! SetLinesForFirenvim(timer) abort
  set lines=25
endfunction
function! OnUIEnter(event)
  if s:IsFirenvimActive(a:event)
    set nolist
    call timer_start(500, function("SetLinesForFirenvim"))
  endif
endfunction

if exists("g:started_by_firenvim")
  " set guifont=JetBrains\ Mono\ Medium:h9
  set guifont=Iosevka\ Term:h9
  " set linespace=-3

  if !exists("g:rko_already_defined_delayed_write_fn")
    let g:rko_already_defined_delayed_write_fn=1
    let g:dont_write = v:false
    function! My_Write(timer) abort
      let g:dont_write = v:false
      silent! update
    endfunction

    function! Delay_My_Write() abort
      if g:dont_write
        return
      end
      let g:dont_write = v:true
      call timer_start(10000, 'My_Write')
    endfunction
  endif

  nnoremap <CR> :w<CR>ZZ
  augroup firenvim
    au!
    autocmd UIEnter * call OnUIEnter(deepcopy(v:event))
    au TextChanged * ++nested call Delay_My_Write()
    au TextChangedI * ++nested call Delay_My_Write()
    au BufEnter localhost_lab*.txt setf python
  augroup END
endif
