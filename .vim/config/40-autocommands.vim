set cursorline    " needed as netrw uses the global value to save and restore state
set cursorcolumn  " needed as netrw uses the global value to save and restore state
augroup CursorLine
  au!
  au VimEnter,WinEnter,BufWinEnter * setlocal cursorline
  au VimEnter,WinEnter,BufWinEnter * setlocal cursorcolumn
  au WinLeave * setlocal nocursorline
  au WinLeave * setlocal nocursorcolumn
  "au VimEnter,WinEnter,BufWinEnter * setlocal cursorcolumn
  "au WinLeave * setlocal nocursorcolumn
augroup END

augroup HardcoreAutoChdir
  au!
  autocmd BufEnter * silent! lcd %:p:h
  autocmd BufEnter * silent! cd %:p:h
augroup END

augroup SaveAllBuffersWhenLosingFocus
  au!
  au FocusLost * silent! wall
augroup END

if !has("gui_running")
  augroup HackToForceAutoreadToWorkCorrectlyInConsoleVim
    au!
    au FocusGained,BufEnter * :silent! !
  augroup END
endif

" Make sure Vim returns to the same line when you reopen a file.
" Thanks, Amit
augroup ReturnToSameLineWhenReopeningFile
  au!
  au BufReadPost *
      \ if line("'\"") > 0 && line("'\"") <= line("$") |
      \     execute 'normal! g`"zv' |
      \     call CenterCursorAesthetically() |
      \ endif

  au BufReadPost COMMIT_EDITMSG exe 'normal! gg'
augroup END

function! SaveAndCheckIfModified()
  if &modified && !&readonly && len(bufname('%')) > 0
    update
    " too distracting (flickering) and slow
    "SyntasticCheck
  endif
endfunction

" this probably causes more trouble than it is worth, especially for files not
" under version control
"
" but I am to lazy and often don't want to press Enter to save...
augroup SaveWhenExitingInsertMode
  au!
  au InsertLeave * call SaveAndCheckIfModified()
augroup END

function! StripTrailingWhitespace()
    let l:my_saved_winview = winsaveview()
    silent! %s/\s\+$//
    call winrestview(l:my_saved_winview)
endfunction
command! StripTrailingWhitespace call StripTrailingWhitespace()
" generates too many annoying deltas in open source projects like OGRE
"augroup StripTrailingWhitespaceOnSave
    "au!
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
"augroup END
"augroup SaveAndRestoreFolds
    "au!
    "au BufWinLeave * silent! mkview
    "au BufWinEnter * silent! loadview
"augroup END
"augroup LocationListAutoOpenClose
    "au!
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
"augroup END
"augroup AlwaysOpenHelpInTheSameWindow
    "autocmd FileType help :wincmd H
"augroup END

"function! AdjustWindowHeight(minheight, maxheight)
  "exe max([min([line("$"), a:maxheight]), a:minheight]) . "wincmd _"
"endfunction
"augroup QuickFixAutoSizer
    "au!
    "au FileType qf call AdjustWindowHeight(3, 16)
"augroup END
