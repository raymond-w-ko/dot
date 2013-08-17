let s:project_directories_list = [
    \ 'C:\cygwin\home\root\src\vim\src',
    \ 'C:\cygwin\home\root\src\ocularwm',
    \ 'C:\cygwin\home\rko\src\vim\src',
    \ 'C:\cygwin\home\rko\src\ocularwm',
    \ 'C:\cygwin\home\root\lib\dot\vim\bundle\omegacomplete',
    \ 'C:\SVN\Syandus_ALIVE3\Frameworks\Carbon',
    \ 'C:\SVN\Syandus_ALIVE3\Frameworks\CarbonCME',
    \ 'C:\SVN\Syandus_ALIVE3\Frameworks\Oxygen',
    \ 'C:\SVN\Syandus_ALIVE3\Groundhog\Client',
    \ 'C:\SVN\Syandus_ALIVE3\Groundhog\ConnectionTester',
    \ 'C:\SVN\Syandus_ALIVE3\Groundhog\Server',
    \ 'C:\SVN\Syandus_ALIVE3\Groundhog\Shared',
    \ 'C:\SVN\Syandus_ALIVE3\Hub\Source',
    \ 'C:\SVN\Syandus_ALIVE3\Hub\Web',
    \ 'C:\SVN\Syandus_ALIVE3\Hub\Web\galleries\cme',
    \ 'C:\SVN\Syandus_ALIVE3\Installation Suite\trunk',
    \ 'C:\SVN\Syandus_ALIVE3\Mac\trunk\ALIVE Med',
    \ 'C:\SVN\Syandus_ALIVE3\Metrics\SyLoginParser',
    \ 'C:\SVN\Syandus_ALIVE3\Metrics\SyMetrics',
    \ 'C:\SVN\Syandus_ALIVE3\Metrics\web',
    \ 'C:\SVN\Syandus_ALIVE3\Platform\Source\Code',
    \ 'C:\SVN\Syandus_ALIVE3\Tools\Source\Launcher',
    \ 'C:\SVN\Syandus_ALIVE3\Tools\Source\SyHandleGen',
    \ 'C:\SVN\Syandus_ALIVE3\Tools\Source\SyRefresh',
    \ 'C:\SVN\Syandus_ALIVE4\Frameworks\Carbon',
    \ 'C:\SVN\Syandus_ALIVE4\Frameworks\Oxygen',
    \ 'C:\SVN\Syandus_ALIVE4\Platform\Source\Code',
    \ 'C:\SVN\Syandus_ALIVE4\Tools\Source\SyProjectGenerator',
    \ 'C:\SVN\Syandus_ALIVE4\Tools\Source\mercky',
    \ 'C:\SVN\Syandus_ALIVE4\Web\Merck\Phase 1\PCRD\retroSyrus',
    \ 'C:\SVN\Syandus_Company\Web\Syandus.com\main\2012-html',
    \ 'C:\SVN\Syandus_Company\Web\Syandus.com\main\2013-html\html',
    \ 'C:\SVN\Syandus_Cores\C_CMSC_MS_01',
    \ 'C:\SVN\Syandus_Cores\C_ImmunoSim_01',
    \ 'C:\SVN\Syandus_Cores\C_Mic_HTN_01',
    \ 'C:\SVN\Syandus_Cores\C_Ogre_Lair_01',
    \ 'C:\SVN\Syandus_Cores\C_Spv_COPD_01',
    \ 'C:\SVN\Syandus_Cores\C_Sut_AE_01',
    \ 'C:\SVN\Syandus_Cores\C_Sym_DM_01',
    \ 'C:\SVN\Syandus_Cores\C_Unb_COPD_01',
    \ ]

let s:project_directories = {}
for directory in s:project_directories_list
    if has('mac') || has ('unix')
        let directory = substitute(directory, '\\', '/', 'g')
        let directory = substitute(directory, 'C:/', $HOME . '/', 'g')
    endif
    let s:project_directories[directory] = 1
endfor

" traverses up directories until it finds one what has 'root.dir'
" it then returns that directory
function! MyGetProjectDirectory()
    let last_directory = ''
    let directory = getcwd()

    while has_key(s:project_directories, directory) == 0 && last_directory != directory
        let last_directory = directory
        let directory = substitute(simplify(directory . '/..'),
                    \ '[\\/]*$', '', '')
    endwhile

    if last_directory == directory
        return getcwd()
    elseif has('win32')
        return directory . '\'
    else
        return directory . '/'
endfunction

let s:cores_with_carbon_lua_shared_framework = [
    \ 'C:/SVN/Syandus_Cores/C_Ogre_Lair_01/Build/Framework/Shared/Scripts',
    \]

function! PropagateCarbonFrameworkLua()
    let errors = getloclist(0)
    if len(errors) > 0
        " don't copy a syntatically incorrect file!
        " assumes you have syntastic installed and it can check Lua
        "return
    endif

    let file = expand('%:p')
    for dir in s:cores_with_carbon_lua_shared_framework
        if !isdirectory(dir)
            continue
        endif
        if has('win32')
            exe 'silent !copy "' . file . '" "' . dir . '"'
        endif
    endfor
endfunction

let s:cores_with_oxygen_lua_shared_framework = [
    \ 'C:/SVN/Syandus_Cores/C_ImmunoSim_01/Build/Framework/Shared/Scripts',
    \]

function! PropagateOxygenFrameworkLua()
    let errors = getloclist(0)
    if len(errors) > 0
        " don't copy a syntatically incorrect file!
        " assumes you have syntastic installed and it can check Lua
        "return
    endif

    let file = expand('%:p')
    for dir in s:cores_with_oxygen_lua_shared_framework
        if !isdirectory(dir)
            continue
        endif
        if has('win32')
            exe 'silent !copy "' . file . '" "' . dir . '"'
        endif
    endfor
endfunction

let s:cores_with_carbon_ogre_shared_framework = [
    \ 'C:/SVN/Syandus_Cores/C_Ogre_Lair_01/Build/Framework/Shared/OGRE',
    \]

function! PropagateCarbonFrameworkOgre()
    let file = expand('%:p')
    for dir in s:cores_with_carbon_ogre_shared_framework
        if !isdirectory(dir)
            continue
        endif
        let subdirs = substitute(file, '\', '/', 'g')
        let subdirs = substitute(
            \ subdirs,
            \ 'C:/SVN/Syandus_ALIVE4/Frameworks/Carbon/Build/Content/OGRE/',
            \ '', '')
        if has('win32')
            exe 'silent !copy "' . file . '" "' . dir . '/' . subdirs . '"'
        endif
    endfor
endfunction

let s:cores_with_oxygen_ogre_shared_framework = [
    \ 'C:/SVN/Syandus_Cores/C_ImmunoSim_01/Build/Framework/Shared/OGRE',
    \]

function! PropagateOxygenFrameworkOgre()
    let file = expand('%:p')
    for dir in s:cores_with_oxygen_ogre_shared_framework
        if !isdirectory(dir)
            continue
        endif
        let subdirs = substitute(file, '\', '/', 'g')
        let subdirs = substitute(
            \ subdirs,
            \ 'C:/SVN/Syandus_ALIVE4/Frameworks/Oxygen/Build/Content/OGRE/',
            \ '', '')
        if has('win32')
            exe 'silent !copy "' . file . '" "' . dir . '/' . subdirs . '"'
        endif
    endfor
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

" executes the specificed autohotkey script
let s:username = expand('$USERNAME')
function! AutoHotkeyMake(arg0)
    if filereadable("C:/Users/root/Desktop/Dropbox/make.ahk")
        let file = "C:/Users/root/Desktop/Dropbox/make.ahk"
    elseif filereadable("C:/Users/Raymond W. Ko/Dropbox/make.ahk")
        let file = "C:/Users/Raymond W. Ko/Dropbox/make.ahk"
    else
        echom "autohotkey script not found"
    endif
    silent! execute ':!start "C:/Program Files/AutoHotkey/AutoHotkey.exe" ' .
        \ '"' . file . '" ' .
        \ a:arg0
endfunction

function! AutoHotkeyConsole2Make(console_name, cmd)
    if filereadable("C:/Users/root/Desktop/Dropbox/console2_make.ahk")
        let file = "C:/Users/root/Desktop/Dropbox/console2_make.ahk"
    elseif filereadable("C:/Users/Raymond W. Ko/Dropbox/console2_make.ahk")
        let file = "C:/Users/Raymond W. Ko/Dropbox/console2_make.ahk"
    else
        echom "autohotkey script not found"
    endif
    silent! execute ':!start "C:/Program Files/AutoHotkey/AutoHotkey.exe" ' .
        \ '"' . file . '" ' .
        \ a:console_name . ' ' . a:cmd
endfunction

function! AutoHotkeyWinSCP(arg0)
    if filereadable("C:/Users/root/Desktop/Dropbox/winscp_sync.ahk")
        let file = "C:/Users/root/Desktop/Dropbox/winscp_sync.ahk"
    elseif filereadable("C:/Users/Raymond W. Ko/Dropbox/winscp_sync.ahk")
        let file = "C:/Users/Raymond W. Ko/Dropbox/winscp_sync.ahk"
    else
        echom "autohotkey script not found"
    endif
  silent! execute ':!start "C:/Program Files/AutoHotkey/AutoHotkey.exe" ' .
        \ '"' . file . '" ' .
        \ a:arg0
endfunction

" ex command for toggling hex mode - define mapping if desired
command! -bar HexMode call ToggleHex()

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
    let &ft="xxd"
    " set status
    let b:editHex=1
    " switch to hex editor
    %!xxd
    %:s/$//g
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

function! PrecedingWhitespaceCount(line)
    let num_space = 0
    for i in range(0, strlen(a:line))
        if (match(a:line[i], '\v\W') != -1)
            let num_space = num_space + 1
        else
            break
        endif
    endfor

    return num_space
endfunction

" vim:fdm=marker:foldlevel=0
