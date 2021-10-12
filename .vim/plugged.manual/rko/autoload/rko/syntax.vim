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

fun! rko#syntax#apply_old_basic_highlights()
  syntax clear
  silent! syntax clear rkoBasicString
  silent! syntax clear rkoBasicComment
  silent! syntax clear rkoMultiLineString
  silent! syntax clear rkoVersionControlDelete
  silent! syntax clear rkoVersionControlAdd
  silent! syntax clear gitMergeConflict

  hi LimeGreen guifg=#00ff00 guibg=#002b36 gui=none ctermbg=0 ctermfg=46 term=none cterm=none

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

  if has_key(s:version_control_filetypes, &filetype)
    syntax region rkoVersionControlDelete start=/\v^-/ end=/\v$/
    syntax region rkoVersionControlAdd start=/\v^\+/ end=/\v$/
  endif
  highlight link rkoVersionControlDelete DiffDelete
  highlight link rkoVersionControlAdd DiffAdd
  highlight link rkoTODO Define

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
endf

fun! rko#syntax#apply_basic_highlights() abort
  syntax clear

  hi LimeGreen guifg=#00ff00 guibg=#002b36 gui=none ctermbg=0 ctermfg=46 term=none cterm=none

  hi ALESignColumnWithErrors guifg=#ffffff guibg=#ff0000
  hi ALESignColumnWithoutErrors guifg=#ffffff guibg=#ffff00
  hi SignColumn guifg=#ff0000

  if &filetype == "vim"
    runtime! syntax/vim.vim
    runtime! syntax/vim/generated.vim
  elseif &filetype == "gitcommit"
    runtime! syntax/gitcommit.vim
  elseif &filetype == "dirvish"
    runtime! syntax/dirvish.vim
  endif
endf

fun! rko#syntax#apply_custom_highlights() abort
  hi LimeGreen guifg=#00ff00 guibg=#002b36 gui=none ctermbg=0 ctermfg=46 term=none cterm=none

  hi link rkoError Define
  hi link rkoTODO LimeGreen

  " hi link clojureTSSymbol Keyword
  " hi link clojureTSKeywordFunction clojureTSFuncMacro

  syntax match rkoTODO /\v<TODO|TODO:|XXX|XXX:|NOTE|NOTE:|WARN|WARN:>/ containedin=ALL
  syntax match rkoError /\verror/ containedin=ALL

  " highlight link gitMergeConflict Error
  syntax match gitMergeConflict /^=======$/ containedin=ALL
  syntax match gitMergeConflict /^<<<<<<< .\+$/ containedin=ALL
  syntax match gitMergeConflict /^>>>>>>> .\+$/ containedin=ALL
endf


