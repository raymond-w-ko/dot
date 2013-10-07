let s:username = expand('$USERNAME')
if s:username == 'Raymond W. Ko'
  let s:cygwin_username = 'rko'
else
  let s:cygwin_username = 'root'
endif

if has('win32')
  let s:unix_home = 'C:/cygwin/home/__CYGWINUSERNAME__'
  let s:unix_home = substitute(s:unix_home, '__CYGWINUSERNAME__', s:cygwin_username, '')
elseif has('win32unix')
  let s:unix_home = '~'
else
  let s:unix_home = '~'
endif

function! MyTranslateDirectory(dir)
  let dir = a:dir
  if has('win32')
    let dir = substitute(dir, '__SVN__', 'C:', '')
  elseif has('win32unix')
    let dir = substitute(dir, '__SVN__', '/cygdrive/c', '')
  else
    let dir = substitute(dir, '__SVN__', '~', 'g')
  endif

  let dir = substitute(dir, '__UNIXHOME__', s:unix_home, 'g')

  let dir = substitute(dir, '__USERNAME__', s:username, '')
  let dir = substitute(dir, ' ', '\ ', '')
  if match(dir, 'Dropbox') != -1
    if !isdirectory(dir)
      let dir = substitute(dir, '/Desktop', '', '')
    endif
  endif

  if has('win32')
    let dir = substitute(dir, '/', '\\', 'g')
  endif

  return dir
endfunction

let s:project_directories_list = [
    \ '__UNIXHOME__/src/ocularwm',
    \ '__UNIXHOME__/src/vim/src',
    \ '__UNIXHOME__/src/ocularwm',
    \ '__UNIXHOME__/lib/dot/vim/bundle/omegacomplete',
    \ '__SVN__/SVN/Syandus_ALIVE3/Frameworks/Carbon',
    \ '__SVN__/SVN/Syandus_ALIVE3/Frameworks/CarbonCME',
    \ '__SVN__/SVN/Syandus_ALIVE3/Frameworks/Oxygen',
    \ '__SVN__/SVN/Syandus_ALIVE3/Groundhog/Client',
    \ '__SVN__/SVN/Syandus_ALIVE3/Groundhog/ConnectionTester',
    \ '__SVN__/SVN/Syandus_ALIVE3/Groundhog/Server',
    \ '__SVN__/SVN/Syandus_ALIVE3/Groundhog/Shared',
    \ '__SVN__/SVN/Syandus_ALIVE3/Hub/Source',
    \ '__SVN__/SVN/Syandus_ALIVE3/Hub/Web',
    \ '__SVN__/SVN/Syandus_ALIVE3/Hub/Web/galleries/cme',
    \ '__SVN__/SVN/Syandus_ALIVE3/Installation Suite/trunk',
    \ '__SVN__/SVN/Syandus_ALIVE3/Mac/trunk/ALIVE Med',
    \ '__SVN__/SVN/Syandus_ALIVE3/Metrics/SyLoginParser',
    \ '__SVN__/SVN/Syandus_ALIVE3/Metrics/SyMetrics',
    \ '__SVN__/SVN/Syandus_ALIVE3/Metrics/web',
    \ '__SVN__/SVN/Syandus_ALIVE3/Platform/Source/Code',
    \ '__SVN__/SVN/Syandus_ALIVE3/Tools/Source/Launcher',
    \ '__SVN__/SVN/Syandus_ALIVE3/Tools/Source/SyHandleGen',
    \ '__SVN__/SVN/Syandus_ALIVE3/Tools/Source/SyRefresh',
    \ '__SVN__/SVN/Syandus_ALIVE4/Frameworks/Carbon',
    \ '__SVN__/SVN/Syandus_ALIVE4/Frameworks/Oxygen',
    \ '__SVN__/SVN/Syandus_ALIVE4/Platform/Source/Code',
    \ '__SVN__/SVN/Syandus_ALIVE4/Tools/Source/SyProjectGenerator',
    \ '__SVN__/SVN/Syandus_ALIVE4/Tools/Source/mercky',
    \ '__SVN__/SVN/Syandus_ALIVE4/Web/Merck/Phase 1/PCRD/retroSyrus',
    \ '__SVN__/SVN/Syandus_Company/Web/Syandus.com/main/2012-html',
    \ '__SVN__/SVN/Syandus_Company/Web/Syandus.com/main/2013-html/html',
    \ '__SVN__/SVN/Syandus_Cores/C_CMSC_MS_01',
    \ '__SVN__/SVN/Syandus_Cores/C_ImmunoSim_01',
    \ '__SVN__/SVN/Syandus_Cores/C_Mic_HTN_01',
    \ '__SVN__/SVN/Syandus_Cores/C_Ogre_Lair_01',
    \ '__SVN__/SVN/Syandus_Cores/C_Spv_COPD_01',
    \ '__SVN__/SVN/Syandus_Cores/C_Sut_AE_01',
    \ '__SVN__/SVN/Syandus_Cores/C_Sym_DM_01',
    \ '__SVN__/SVN/Syandus_Cores/C_Unb_COPD_01',
    \ ]

let g:my_project_directories = {}
for directory in s:project_directories_list
  let dir = MyTranslateDirectory(directory)
  let g:my_project_directories[dir] = 1
endfor

" traverses up directories until it finds one what has 'root.dir'
" it then returns that directory
function! MyGetProjectDirectory()
  let last_directory = ''
  let directory = getcwd()

  while has_key(g:my_project_directories, directory) == 0 && last_directory != directory
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
