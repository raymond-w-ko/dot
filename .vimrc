set nocompatible
set noswapfile
set nobackup
filetype off
syntax on
filetype plugin indent on
set modelines=0
set number
set ruler
set encoding=utf-8
set wrap
set textwidth=99
set tabstop=2
set shiftwidth=2
set softtabstop=2
set expandtab
set hidden
set ttyfast
set laststatus=2
set showmode
set showcmd
set listchars=tab:▸\ ,eol:¬

let mapleader = " "

nnoremap j gj
nnoremap k gk

nnoremap / /\v
vnoremap / /\v
set hlsearch
set incsearch
set ignorecase
set smartcase
set showmatch

map <leader><space> :let @/=''<cr>
map <leader>q gqip

"set t_Co=256
" set Vim-specific sequences for RGB colors
let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
set termguicolors
set background=dark
colorscheme selenized
