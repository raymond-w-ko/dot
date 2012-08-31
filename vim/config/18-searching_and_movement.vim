" use aesthetic middle of screen for "zz"
function! AestheticCenterCursor()
    normal! zz

    let center = round(winheight(0) / 2.0)
    let offset = winheight(0) * 0.1
    let final = center - offset
    let rounded_final = float2nr(final)
    let rounded_offset = float2nr(offset)

    let delta = winline() - (rounded_final + 1)

    if (delta <= 0)
        return
    endif

    exe 'normal ' . delta . "\<C-e>"

    return
endfunction
nnoremap <silent> zz :call AestheticCenterCursor()<CR>

" Searching and movement
" Use sane regexes.
nnoremap / /\v
vnoremap / /\v

nnoremap ? ?\v
vnoremap ? ?\v

set ignorecase
set smartcase
set hlsearch
set incsearch
set gdefault            " inverts the meaning of the g-flag in s///g

set scrolloff=0
set sidescroll=0
set sidescrolloff=0

set virtualedit+=block

nnoremap <silent> <leader><Space> :nohlsearch<CR>:let @/=''<CR>

nmap n nzzzv
nmap N Nzzzv

nmap G Gzz

" Don't move on *
"nnoremap * *<c-o>
nnoremap <silent> * :set nohls<CR>:let @/='\<<C-R>=expand('<cword>')<CR>\>'<CR>:set hls<CR>

" Same when jumping around
"nnoremap g; g;zz
"nnoremap g, g,zz

" Easier to type, and I never use the default behavior.
nnoremap H ^
nnoremap L g_

" Open a Quickfix window for the last search.
nnoremap <silent> <leader>/ :execute 'lvimgrep /'.@/.'/g %'<CR>:lopen<CR>

" Ack for the last search.
"nnoremap <silent> <leader>? :execute "Ack! '" . substitute(substitute(substitute(@/, "\\\\<", "\\\\b", ""), "\\\\>", "\\\\b", ""), "\\\\v", "", "") . "'"<CR>

" Fix linewise visual selection of various text objects
nnoremap VV V
nnoremap Vit vitVkoj
nnoremap Vat vatV
nnoremap Vab vabV
nnoremap VaB vaBV

" It's 2011.
nnoremap <silent> j gj
nnoremap <silent> k gk

" Easy buffer navigation
nnoremap <C-h>  <C-w>h
nnoremap <C-j>  <C-w>j
nnoremap <C-k>  <C-w>k
nnoremap <C-l>  <C-w>l

" treat leading whitespace as though it was not there
function! MyLeftBrace()
    let orig_wrapscan=&wrapscan
    let temp=@/
    set nohls
    set nowrapscan
    ?^\s*$
    let @/=temp
    set hls
    if (orig_wrapscan)
        set wrapscan
    endif
endfunction
nnoremap <silent> { :silent! call MyLeftBrace()<CR>

function! MyRightBrace()
    let orig_wrapscan=&wrapscan
    let temp=@/
    set nohls
    set nowrapscan
    /^\s*$
    let @/=temp
    set hls
    if (orig_wrapscan)
        set wrapscan
    endif
endfunction
nnoremap <silent> } :silent! call MyRightBrace()<CR>

function! PushBraceSettings()
    let g:BraceSettingsOrigWrapscan=&wrapscan
    let g:BraceSettingsOrigSearch=@/

    set nohls
    set nowrapscan

    return ''
endfunction
function! PopBraceSettings()
    let @/=g:BraceSettingsOrigSearch
    set hls

    if (g:BraceSettingsOrigWrapscan)
        set wrapscan
    endif

    return ''
endfunction

vnoremap <silent> { ?<C-r>=PushBraceSettings()<CR>^\s*$<CR><ESC>:<C-r>=PopBraceSettings()<CR><ESC>gv
vnoremap <silent> } /<C-r>=PushBraceSettings()<CR>^\s*$<CR><ESC>:<C-r>=PopBraceSettings()<CR><ESC>gv

" Highlight word {{{
"nnoremap <silent> <leader>hh :execute 'match InterestingWord1 /\<<c-r><c-w>\>/'<cr>
"nnoremap <silent> <leader>h1 :execute 'match InterestingWord1 /\<<c-r><c-w>\>/'<cr>
"nnoremap <silent> <leader>h2 :execute '2match InterestingWord2 /\<<c-r><c-w>\>/'<cr>
"nnoremap <silent> <leader>h3 :execute '3match InterestingWord3 /\<<c-r><c-w>\>/'<cr>
" }}}

" Visual Mode */# from Scrooloose {{{
function! s:VisualModeSetSearch()
  let temp = @@
  norm! gvy
  let @/ = '\V' . substitute(escape(@@, '\'), '\n', '\\n', 'g')
  let @@ = temp
endfunction

vnoremap * :<C-u>call <SID>VisualModeSetSearch()<CR>//<CR><c-o>
vnoremap # :<C-u>call <SID>VisualModeSetSearch()<CR>??<CR><c-o>
