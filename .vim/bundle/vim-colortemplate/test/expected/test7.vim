" Name:         Test 7
" Author:       y
" Maintainer:   w
" License:      Vim License (see `:help license`)

set background=dark

hi clear
if exists('syntax_on')
  syntax reset
endif

let g:colors_name = 'test7'

let s:t_Co = exists('&t_Co') && !empty(&t_Co) && &t_Co > 1 ? &t_Co : 2

if (has('termguicolors') && &termguicolors) || has('gui_running')
  hi Normal guifg=#ffffff guibg=#000000 guisp=NONE gui=reverse cterm=reverse
  unlet s:t_Co
  finish
endif

if s:t_Co >= 256
  hi Normal ctermfg=255 ctermbg=16 cterm=reverse
  if !has('patch-8.0.0616') " Fix for Vim bug
    set background=dark
  endif
  unlet s:t_Co
  finish
endif

" vim: et ts=2 sw=2
