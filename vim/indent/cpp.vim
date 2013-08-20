" Vim indent file
" Language:	C++
" Last Modified By: Raymond W. Ko <raymond.w.ko@gmail.com>
" Maintainer:	Konstantin Lepa <konstantin.lepa@gmail.com>
" Last Change:	2013 August 17
" License: MIT
" Version: 1.1.0
"
" Changes {{{
" 1.1.0 2011-01-17
"   Refactored source code.
"   Some fixes.
"
" 1.0.1 2010-05-20
"   Added some changes. Thanks to Eric Rannaud <eric.rannaud@gmail.com>
"
"}}}

if exists("b:did_indent")
  finish
endif
let b:did_indent = 1

function! GoogleCppIndentFindLineWithOpenLeftBrace()
  let line_num = line('.') - 1
  let lcount = 0
  let rcount = 0
  while line_num > 0
    let line = getline(line_num)
    let ind = len(line) - 1
    while ind >= 0
      if line[ind] == '{'
        let lcount += 1
        if lcount > rcount
          return line_num
        endif
      elseif line[ind] == '}'
        let rcount += 1
      endif
      let ind -= 1
    endwhile
    let line_num -= 1
  endwhile
  return 0
endfunction

function! GoogleCppIndent()
  let l:cline_num = line('.')

  let l:orig_indent = cindent(l:cline_num)

  if l:orig_indent == 0 | return 0 | endif

  let l:pline_num = prevnonblank(l:cline_num - 1)
  let l:pline = getline(l:pline_num)
  let l:ppline = getline(l:pline_num - 1)
  let l:cline = getline(l:cline_num)
  if l:pline =~# '^\s*template' 
    return l:pline_indent 
  endif

  if l:cline =~ '\s*}\s*'
    let l:brace_line = GoogleCppIndentFindLineWithOpenLeftBrace()
    if l:brace_line != 0 && getline(l:brace_line - 1) =~ '.*),\s*'
      return l:orig_indent - 2 - (2 * &shiftwidth)
    else
      return l:orig_indent
    endif
  end

  " TODO: I don't know to correct it:
  " namespace test {
  " void
  " ....<-- invalid cindent pos
  "
  " void test() {
  " }
  "
  " void
  " <-- cindent pos
  if l:orig_indent != &shiftwidth 
    " tries to indent initalizer list in the form of
    " className::classname(...)
    "     : var1(value1),
    "       var2(value2)
    " and
    " classname(...)
    "     : var1(value1),
    "       var2(value2)
    if l:pline =~ '^\s\+:.*,\s*$'
      return l:orig_indent + 2
    elseif l:pline =~ '.*)\s*{\s*' && l:ppline =~ '.*,\s*$'
      return l:orig_indent - 2 - (2 * &shiftwidth)
    else
      return l:orig_indent
    endif
  endif

  let l:in_comment = 0
  let l:pline_num = prevnonblank(l:cline_num - 1)
  while l:pline_num > -1
    let l:pline = getline(l:pline_num)
    let l:ppline = getline(l:pline_num - 1)
    let l:pline_indent = indent(l:pline_num)

    if l:in_comment == 0 && l:pline =~ '^.\{-}\(/\*.\{-}\)\@<!\*/'
      let l:in_comment = 1
    elseif l:in_comment == 1
      if l:pline =~ '/\*\(.\{-}\*/\)\@!'
        let l:in_comment = 0
      endif
    elseif l:pline_indent == 0
      if l:pline !~# '\(#define\)\|\(^\s*//\)\|\(^\s*{\)'
        if l:pline =~# '^\s*namespace.*'
          return 0
        elseif l:cline =~ '^\s*:.*$'
          return l:orig_indent + &shiftwidth
        else
          return l:orig_indent
        endif
      elseif l:pline =~# '\\$'
        return l:orig_indent
      endif
    else
      return l:orig_indent
    endif

    let l:pline_num = prevnonblank(l:pline_num - 1)
  endwhile

  return l:orig_indent
endfunction

"setlocal shiftwidth=2
"setlocal tabstop=2
"setlocal softtabstop=2
setlocal expandtab
"setlocal textwidth=80
"setlocal wrap

setlocal cindent
setlocal cinoptions=h1,l1,g1,t0,i4,+4,(0,w1,W4
setlocal indentkeys=0{,=},:,0#,!^F,o,O,e

setlocal indentexpr=GoogleCppIndent()

let b:undo_indent = "setl sw< ts< sts< et< tw< wrap< cin< cino< inde<"
