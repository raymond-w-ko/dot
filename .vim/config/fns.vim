fun! Rko_toggle_linter() abort
  if g:ale_echo_delay <= 10
    " OFF
    echom "ALE popup off"
    let g:ale_echo_delay = 100000 
    if has("nvim")
      let g:ale_hover_to_floating_preview=0
      let g:ale_detail_to_floating_preview=0
      let g:ale_floating_preview=0
      let g:ale_cursor_detail=0
    endif
  else
    " ON
    echom "ALE popup on"
    let g:ale_echo_delay = 0
    if has("nvim")
      let g:ale_hover_to_floating_preview=1
      let g:ale_detail_to_floating_preview=1
      let g:ale_floating_preview=1
      let g:ale_cursor_detail=1
    endif
  endif
endfun
