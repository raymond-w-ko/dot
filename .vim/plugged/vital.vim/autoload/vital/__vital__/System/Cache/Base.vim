let s:save_cpo = &cpo
set cpo&vim

function! s:_vital_loaded(V) abort
  let s:Prelude = a:V.import('Prelude')
endfunction
function! s:_vital_depends() abort
  return ['Prelude']
endfunction

let s:cache = {
      \ '__name__': 'base',
      \}
function! s:new(...) abort
  return deepcopy(s:cache)
endfunction
function! s:cache.cache_key(obj) abort
  let cache_key = s:Prelude.is_string(a:obj) ? a:obj : string(a:obj)
  return cache_key
endfunction
" @vimlint(EVL103, 1, a:name)
" @vimlint(EVL103, 1, a:value)
function! s:cache.has(name) abort
  throw 'vital: System.Cache.Base: has({name}) is not implemented'
endfunction
function! s:cache.get(name, ...) abort
  throw 'vital: System.Cache.Base: get({name}[, {default}]) is not implemented'
endfunction
function! s:cache.set(name, value) abort
  throw 'vital: System.Cache.Base: set({name}, {value}[, {default}]) is not implemented'
endfunction
function! s:cache.keys() abort
  throw 'vital: System.Cache.Base: keys() is not implemented'
endfunction
function! s:cache.remove(name) abort
  throw 'vital: System.Cache.Base: remove({name}) is not implemented'
endfunction
function! s:cache.clear() abort
  throw 'vital: System.Cache.Base: clear() is not implemented'
endfunction
" @vimlint(EVL103, 0, a:value)
" @vimlint(EVL103, 0, a:name)
function! s:cache.on_changed() abort
  " A user defined hook function
endfunction

let &cpo = s:save_cpo
unlet s:save_cpo
"vim: sts=2 sw=2 smarttab et ai textwidth=0 fdm=marker
