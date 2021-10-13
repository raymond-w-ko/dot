" vim:fdm=marker

set encoding=utf-8 " http://utf8everywhere.org/
" speed hacks
let g:loaded_rrhelper=1
let g:did_install_default_menus=1 " avoid stupid menu.vim (saves ~100ms)

let s:use_treesitter=has("nvim")

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
let mapleader = "\<space>"
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
Plug '$HOME/dot/.vim/plugged.manual/rko'
nnoremap <leader>o :ToggleWord<CR>

if has("nvim")
  Plug 'glacambre/firenvim'
endif

" theme utils
Plug 'rktjmp/lush.nvim'
Plug 'lifepillar/vim-colortemplate'
" themes
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'jacoborus/tender.vim'
Plug 'romainl/Apprentice'
Plug 'kvrohit/substrata.nvim'
Plug 'raymond-w-ko/snow'
Plug 'raymond-w-ko/seabird'
Plug 'https://github.com/RRethy/nvim-base16'
Plug 'https://github.com/ellisonleao/gruvbox.nvim'

" my plugins
" Plug 'raymond-w-ko/vim-solarized8'
" Plug 'raymond-w-ko/scrollfix'
" let g:scrollfix=50
Plug 'raymond-w-ko/vim-eslisp'
Plug 'raymond-w-ko/vim-lua-indent'

Plug 'raymond-w-ko/vim-geckocomplete'
inoremap <silent><expr><nowait> <C-s> geckocomplete#toggle_pause_completion()
inoremap <silent><expr><nowait> <Tab> geckocomplete#completion_key()

" finders
" Plug 'ctrlpvim/ctrlp.vim'
" Plug 'FelikZ/ctrlp-py-matcher'
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'
" CTRL-A CTRL-Q to select all and build quickfix list

fun! s:build_quickfix_list(lines)
  call setqflist(map(copy(a:lines), '{ "filename": v:val }'))
endf
let g:fzf_action = {
    \ 'ctrl-q': function('s:build_quickfix_list'),
    \ 'ctrl-t': 'tab split',
    \ 'ctrl-x': 'split',
    \ 'ctrl-v': 'vsplit' }
let $FZF_DEFAULT_OPTS =
    \ '--bind ctrl-a:select-all ' .
    \ '--color dark,hl:#00ff00,hl+:#00ff00,fg+:235,bg+:#000000,fg+:#bbbbbb ' .
    \ '--color info:6,prompt:3,spinner:9,pointer:46,marker:46,header:46'

fun! s:rebind_rg_cmd() abort
  command! -buffer -bang -nargs=* Rg call fzf#vim#grep("rg --column --line-number --no-heading --color=never --smart-case -- ".shellescape(<q-args>), 1, fzf#vim#with_preview(), <bang>0)
endfun
augroup rebind_rg_cmd
  autocmd BufReadPost * call s:rebind_rg_cmd()
  autocmd BufEnter * call s:rebind_rg_cmd()
augroup END

" potpourri
Plug 'vim-jp/vital.vim'
Plug 'kana/vim-operator-user'
Plug 'qpkorr/vim-bufkill'
let g:BufKillCreateMappings = 0
Plug 'Konfekt/FastFold'
Plug 'itchyny/lightline.vim'
command! LightlineReload call LightlineReload()
function! LightlineReload()
  call lightline#init()
  call lightline#colorscheme()
  call lightline#update()
endfunction
if has('python3') || has('python')
  Plug 'SirVer/ultisnips'
endif
Plug 'honza/vim-snippets'
Plug 'dense-analysis/ale'
nnoremap <leader>n :ALENext<CR>
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
      \ nnoremap <silent><buffer> t :call dirvish#open('tabedit', 0)<CR>
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
" highlight Sneak guifg=magenta guibg=black ctermfg=black ctermbg=red
" highlight SneakScope guifg=black guibg=#00ff00 ctermfg=black ctermbg=green
" highlight SneakLabel guifg=magenta guibg=black ctermfg=black ctermbg=red
" highlight SneakLabelMask guifg=black guibg=black ctermfg=black ctermbg=black

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

" misc filetypes
Plug 'octol/vim-cpp-enhanced-highlight'
Plug 'aklt/plantuml-syntax'
Plug 'rhysd/vim-clang-format'
let g:clang_format#code_style="google"
let g:clang_format#detect_style_file=1

if has("nvim") && s:use_treesitter
  " :TSUpdate
  Plug 'nvim-treesitter/nvim-treesitter'
  Plug 'raymond-w-ko/nvim-treesitter'
  Plug 'p00f/nvim-ts-rainbow'
  Plug 'nvim-treesitter/playground'
endif

call plug#end()

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" load sensible defaults by our prophet Tim Pope
runtime! plugin/sensible.vim

set scrolloff=0 " scrolloff 0 is needed by scrollfix
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
  set viewdir=$HOME/.local/share/vim/view
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
  set relativenumber
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
set list
set listchars=tab:··,trail:•,extends:>,precedes:<,nbsp:+
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
set nowrap
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
set wildignore+=*.a,*.o
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
  for dir in possible_homes
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

if !exists("g:rko_already_setup_syntax")
  if s:use_treesitter
    syntax enable
  else
    syntax enable
  endif
  let g:rko_already_setup_syntax=1
endif
syntax conceal on
if exists('+termguicolors')
  set termguicolors
  if !has("nvim")
    let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
    let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
  endif
endif
if !exists("g:already_set_color_scheme") && !($TERM == "linux")
  " set background=dark

  " let base16colorspace=256
  " colorscheme preto

  " let g:solarized_italics = 0
  " colorscheme solarized8_flat
  
  " let g:dracula_bold=0
  " let g:dracula_italic=0
  " let g:dracula_underline=0
  " let g:dracula_undercurl=0
  " let g:dracula_inverse=1
  " colorscheme dracula
  
  " colorscheme tender

  " colorscheme apprentice

  " colorscheme petrel
  
  set background=light
  let g:gruvbox_contrast_light="soft"
  colorscheme gruvbox
  

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
set ignorecase
set smartcase
set hlsearch
set gdefault            " inverts the meaning of the g-flag in s///g

set virtualedit+=block
set sidescroll=1
set sidescrolloff=1

nnoremap <silent> <leader>l :nohlsearch<CR>:let @/=''<CR>:call clearmatches()<CR>

fun! s:get_visual_selection()
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
endf
fun! s:selection()
  try
    let a_save = @a
    normal! gv"ay
    return @a
  finally
    let @a = a_save
  endtry
endf
fun! s:rg_handler(file)
  let i = stridx(a:file, ":")
  if i > 0
    exe "edit " . a:file[0:i - 1]
  endif
endf
fun! <SID>find_word_in_project()
  let needle = getreg('"')
  " let needle = escape(needle, ">")
  echom needle
  let dir = rko#get_project_directory()
  call fzf#vim#grep(
      \ "rg --column --line-number --no-heading --color=never --smart-case -- ".shellescape(needle),
      \ 1,
      \ fzf#vim#with_preview()
      \ )
endf
nnoremap <leader>r :call <SID>find_word_in_project()<CR>

" Easier to type, and I never use the default behavior.
nnoremap H ^
nnoremap L g_

" quickfix
nnoremap <leader>co :botright copen<CR>
nnoremap <leader>cc :cclose<CR>

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

nnoremap <silent> { :call rko#left_brace()<CR>
nnoremap <silent> } :call rko#right_brace()<CR>

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

fun! s:map_string_operator() abort
  onoremap is i"
  onoremap as a"
  vnoremap is i"
  vnoremap as a"
endf
call s:map_string_operator()
fun! s:map_buffer_string_operator() abort
  onoremap <buffer> is i"
  onoremap <buffer> as a"
  vnoremap <buffer> is i"
  vnoremap <buffer> as a"
endf

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

inoremap <expr> χ rko#paredit_forward_up()
inoremap <expr> <Right> rko#paredit_forward_up()

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
  fun! MyFindFileInProjectAndEdit(sink)
    let dir = rko#get_project_directory()
    call fzf#run({
        \ "sink": a:sink,
        \ "options": printf('--prompt "%s"', dir),
        \ "dir": dir,
        \ "window": {"width": 0.618, "height": 0.618,},})
  endf

  nnoremap <leader>b :Buffers<CR>
  nnoremap <C-p> :call MyFindFileInProjectAndEdit('edit')<CR>
  nnoremap <leader>et :call MyFindFileInProjectAndEdit('tabedit')<CR>
else
  " use ctrlp.vim
  fun! MyFindFileInProjectAndEdit()
    execute ':CtrlP ' . rko#escape_pathname(rko#get_project_directory())
  endf

  nnoremap <leader>b :CtrlPBuffer<CR>
  nnoremap <C-p> :call MyFindFileInProjectAndEdit()<CR>
endif

nnoremap <leader>a :A<CR>
nmap <leader><leader> <C-^>

" This allows for change paste motion cp{motion}
" http://stackoverflow.com/questions/2471175/vim-replace-word-with-contents-of-paste-buffer
fun! ChangePaste(type, ...)
  silent exe "normal! `[v`]\"_c"
  silent exe "normal! p"
endf
nnoremap <silent> cp :set opfunc=ChangePaste<CR>g@

nnoremap <C-Up> :resize +1<CR>
nnoremap <C-Down> :resize -1<CR>
nnoremap <C-Left> :vertical resize -1<CR>
nnoremap <C-Right> :vertical resize +1<CR>
nnoremap <silent> <S-Left> :call rko#mark_window_swap()<CR><C-w>h:call rko#do_window_swap()<CR>
nnoremap <silent> <S-Right> :call rko#mark_window_swap()<CR><C-w>l:call rko#do_window_swap()<CR>
nnoremap <leader>tt :call rko#create_vsplits()<CR>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" matchparen
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" let g:loaded_matchparen = 1

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

"fun! CtrlPMatch(items, str, limit, mmode, ispath, crfile, regex) abort
  "let items = copy(a:items)
  "if a:ispath
    "call filter(items, 'v:val !=# a:crfile')
  "endif
  "return haystack#filter(items, a:str)
"endf
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

if has("python") || has("python3")
  let g:ctrlp_match_func = { 'match': 'pymatcher#PyMatch' }
endif

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" lightline
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:lightline = {
    \ 'colorscheme': 'gruvbox',
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

fun! LightLineModified()
  return &ft =~ 'help\|vimfiler\|gundo' ? '' : &modified ? '+' : &modifiable ? '' : '-'
endf

fun! LightLineReadonly()
  return &ft !~? 'help\|vimfiler\|gundo' && &readonly ? '[RO]' : ''
  " return &ft !~? 'help\|vimfiler\|gundo' && &readonly ? '⭤' : ''
endf

fun! LightLineShortenedPath()
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
endf

fun! LightLineFilename()
  return ('' != LightLineReadonly() ? LightLineReadonly() . ' ' : '') .
      \ (&ft == 'vimfiler' ? vimfiler#get_status_string() :
      \  &ft == 'unite' ? unite#get_status_string() :
      \  &ft == 'vimshell' ? vimshell#get_status_string() :
      \ '' != expand('%:t') ? LightLineShortenedPath() : '[No Name]') .
      \ ('' != LightLineModified() ? ' ' . LightLineModified() : '')
endf

fun! LightLineFugitive()
  if &ft !~? 'vimfiler\|gundo' && exists("*fugitive#head")
    let _ = fugitive#head()
    return strlen(_) ? '(branch) '._ : ''
    " return strlen(_) ? '⭠ '._ : ''
  endif
  return ''
endf

fun! LightLineFileformat()
  return winwidth(0) > 70 ? &fileformat : ''
endf

fun! LightLineFiletype()
  return winwidth(0) > 70 ? (strlen(&filetype) ? &filetype : 'no ft') : ''
endf

fun! LightLineFileencoding()
  return winwidth(0) > 70 ? (strlen(&fenc) ? &fenc : &enc) : ''
endf

fun! LightLineMode()
  return winwidth(0) > 60 ? lightline#mode() : ''
endf

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
let g:clojure_fuzzy_indent_patterns = ['^with', '^def', '^let', '^go$', '^go-loop$', '^comment$', 'fdef$', '^profile$', '^p$']
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
let g:ale_echo_delay=100000
nnoremap <silent> <leader>i :call rko#toggle_linter()<CR>

let g:ale_virtualtext_cursor=0
let g:ale_virtualtext_delay=500

let g:ale_floating_window_border=['│', '─', '╭', '╮', '╯', '╰']

if has("nvim")
  let g:ale_hover_to_floating_preview=0
  let g:ale_detail_to_floating_preview=0
  let g:ale_echo_cursor=1
  let g:ale_floating_preview=0
  let g:ale_cursor_detail=0
else
  let g:ale_echo_cursor=1
  let g:ale_cursor_detail=0
endif

let g:ale_linters = {
    \ "jsx": ["eslint"],
    \ "c": [],
    \ "cpp": ["clangd"],
    \ "clojure": ["joker"],
    \ "python": ["pycodestyle", "pylint"],
    \ }

" ale_linters/sql/sqllint.vim
" Author: Joe Reynolds <joereynolds952@gmail.co>
" Description: sql-lint for SQL files.
"              sql-lint can be found at
"              https://www.npmjs.com/package/sql-lint
"              https://github.com/joereynolds/sql-lint

fun! ALE_sqldashlint_Handle(buffer, lines) abort
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
endf

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

augroup rko_vimrc
  au!

  " ALL
  au BufWritePost *.vimrc source $MYVIMRC
  au BufWritePost *.gvimrc source $MYGVIMRC
  " reset cursor on start:
  au VimEnter * silent !echo -ne "\e[4 q"
  au FileType * call rko#setup_pair_bindings()

  " only show cursorline if a window has focus
  " this noticably slows down VIM in files with complicated syntax highlighting,
  " like PHP, so disable it for now.
  au VimEnter,WinEnter,BufWinEnter * setlocal cursorline
  au WinLeave * setlocal nocursorline

  au CursorHold,CursorHoldI * :silent! checktime
  " hack for console VIM so that check for changed files work correctly
  au FocusGained,BufEnter,WinEnter,TabEnter * :silent! checktime

  " Make sure Vim returns to the same line when you reopen a file.
  " Thanks, Amit
  au BufReadPost *
      \ if line("'\"") > 0 && line("'\"") <= line("$") |
      \     execute 'normal! g`"zv' |
      "\     call CenterCursorAesthetically() |
      \ endif
  " this however is annoying for git commit messages
  au BufReadPost COMMIT_EDITMSG exe 'normal! gg'

  if s:use_treesitter
    autocmd FileType * nnoremap <buffer> zS :silent call rko#show_syntax_groups()<CR>
  endif

  autocmd CursorMoved * call rko#center_cursor()

  autocmd FileType * call rko#syntax#apply_custom_highlights()
  au BufReadPost * call s:map_buffer_string_operator()

  """"""""""""""""""""""""""""""""""""""""
  " css
  au FileType css set omnifunc=csscomplete#CompleteCSS noci
  au FileType css,less,scss setlocal iskeyword+=-
  au FileType scss nnoremap <buffer> <Leader>f :call rko#format_scss()<CR>
  au FileType css nnoremap <buffer> <Leader>f :call rko#format_css()<CR>

  " clojure
  au FileType clojure nnoremap <buffer> <leader>f :Cljfmt<CR>
  " au FileType clojure nnoremap <buffer> <leader>r :Require<CR>
  " au FileType clojure nnoremap <buffer> <leader>R :Require!<CR>

  " C, ObjC, C++
  autocmd FileType c,cpp,objc nnoremap <buffer><Leader>f :<C-u>ClangFormat<CR>zz
  autocmd FileType c,cpp,objc vnoremap <buffer><Leader>f :ClangFormat<CR>

  au FileType gitcommit setlocal foldlevel=9001

  " help in new tab to avoid interfering with existing tab layout
  au BufEnter *.txt call rko#setup_help_tab()

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
  
  au FileType javascript setlocal iskeyword+=$
  au FileType javascript setlocal cinoptions=g0,N-s,(0,u0,Ws,l1,j1,J1

  au FileType java nnoremap <buffer> <Leader>f :call rko#format_java()<CR>
  au FileType json nnoremap <buffer> <Leader>f :call rko#format_json()<CR>
  au FileType javascript nnoremap <buffer> <Leader>f :call rko#format_js()<CR>
  au FileType javascript.jsx nnoremap <buffer> <Leader>f :call rko#format_js()<CR>
  au FileType javascriptreact nnoremap <buffer> <Leader>f :call rko#format_js()<CR>
  au FileType python nnoremap <buffer> <Leader>f :call rko#format_python()<CR>
  au FileType html nnoremap <buffer> <Leader>f :call rko#format_html()<CR>
  au FileType go nnoremap <buffer> <Leader>f :call rko#format_golang()<CR>
  
  au FileType markdown setlocal textwidth=80

  au BufRead .joker set ft=clojure

  " HACKS
  autocmd BufReadPost *.html hi clear htmlItalic
  autocmd BufReadPost *.html hi clear TSEmphasis
augroup END

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

fun! s:IsFirenvimActive(event) abort
  if !exists('*nvim_get_chan_info')
    return 0
  endif
  let l:ui = nvim_get_chan_info(a:event.chan)
  return has_key(l:ui, 'client') && has_key(l:ui.client, 'name') &&
      \ l:ui.client.name =~? 'Firenvim'
endf
fun! SetLinesForFirenvim(timer) abort
  set lines=25
endf
fun! OnUIEnter(event)
  if s:IsFirenvimActive(a:event)
    call timer_start(500, function("SetLinesForFirenvim"))
  endif
endf

if exists("g:started_by_firenvim")
  " set guifont=JetBrains\ Mono\ Medium:h9
  set guifont=Iosevka\ Term:h9
  " set linespace=-3

  if !exists("g:rko_already_defined_delayed_write_fn")
    let g:rko_already_defined_delayed_write_fn=1
    let g:dont_write = v:false
    fun! My_Write(timer) abort
      let g:dont_write = v:false
      silent! update
    endf

    fun! Delay_My_Write() abort
      if g:dont_write
        return
      end
      let g:dont_write = v:true
      call timer_start(10000, 'My_Write')
    endf
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

let g:ale_java_javac_classpath =
    \ ""
    \ . "/home/rko/.m2/repository/org/lmdbjava/lmdbjava/0.8.2/lmdbjava-0.8.2.jar:"
    \ . "/home/rko/.m2/repository/org/agrona/agrona/1.12.0/agrona-1.12.0.jar:"
    \ . "/home/rko/.m2/repository/joda-time/joda-time/2.10.12/joda-time-2.10.12.jar:"
    \ . "/home/rko/.m2/repository/org/apache/commons/commons-collections4/4.4/commons-collections4-4.4.jar:"
    \ . "/home/rko/.m2/repository/org/apache/commons/commons-lang3/3.12.0/commons-lang3-3.12.0.jar:"
let g:ale_java_javac_sourcepath =
    \ "src/jvm"
