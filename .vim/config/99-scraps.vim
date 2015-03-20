finish

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"" from 00-library
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

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

 executes the specificed autohotkey script
function! AutoHotkeyMake(arg0)
    if filereadable("C:/Users/root/Desktop/Dropbox/make.ahk")
        let file = "C:/Users/root/Desktop/Dropbox/make.ahk"
    elseif filereadable("C:/Users/Raymond W. Ko/Dropbox/make.ahk")
        let file = "C:/Users/Raymond W. Ko/Dropbox/make.ahk"
    elseif filereadable("C:/Users/rko/Dropbox/make.ahk")
        let file = "C:/Users/rko/Dropbox/make.ahk"
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

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"" from 91-projects
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

function! SetSettingsForVisualStudioProject(size_of_tab, name, tags, ...)
    if (a:size_of_tab > 0)
        "execute 'setlocal tabstop=' . a:size_of_tab
        "execute 'setlocal shiftwidth=' . a:size_of_tab 
        "execute 'setlocal softtabstop=' . a:size_of_tab
    endif
    if (len(a:name) == 0)
        execute "nnoremap <buffer> <leader>m <nop>"
    else
        execute "nnoremap <buffer> <leader>m :call AutoHotkeyMake('" .
               \ a:name . "')\<CR>"
    endif
	execute 'setlocal tags=' . a:tags
    if a:0 > 0
        setlocal noexpandtab
    endif
endfunction

function! SetSettingsForWinScpProject(size_of_tab, arg0, tags)
	"execute 'setlocal tabstop=' . a:size_of_tab
	"execute 'setlocal shiftwidth=' . a:size_of_tab 
	"execute 'setlocal softtabstop=' . a:size_of_tab
    execute "nnoremap <buffer> <leader>m :call AutoHotkeyWinSCP('" .
            \ a:arg0 . "')\<CR>"
	execute 'setlocal tags=' . a:tags
endfunction

function! SetConsoleMakeSpaceM(console_name, cmd)
    execute "nnoremap <silent><buffer> <leader>m :update<CR>:call AutoHotkeyConsole2Make('" .
        \ a:console_name . "', '" . a:cmd . "')<CR>"
endfunction

function! SetSpaceM(cmd)
    execute "nnoremap <silent><buffer> <leader>m :w!<CR>:update<CR>:" .
                \ a:cmd .
                \ "<CR>"
endfunction

function! SetSpaceMExternalCmd(cmd)
    execute "nnoremap <silent><buffer> <leader>m :w!<CR>:update<CR>:!start " .
                \ a:cmd .
                \ "<CR>"
endfunction

function! SetSettingsForShaders()
    "setlocal tabstop=4 shiftwidth=4 softtabstop=4
    nnoremap <buffer> <leader>m :update<CR>:!start .\install.bat<CR>
    setlocal tags=
endfunction
function! SetSettingsForClojure()
    "setlocal tabstop=2 shiftwidth=2 softtabstop=2
    setlocal tags=
endfunction

exe 'augroup MyProjectSettings'
exe 'au!'

" follows format of [directory, tab width, VS project name, tags list]
let s:visual_studio_project_settings = [
    \ ['C:/cygwin/home/root/lib/dot/vim/bundle/omegacomplete/*',
    \  2, 'OmegaCompletePythonModule', ''],
    \ ['C:/cygwin/home/rko/lib/dot/vim/bundle/omegacomplete/*',
    \  2, 'OmegaCompletePythonModule', ''],
    \ 
    \
    \ ['C:/SVN/Syandus_ALIVE3/Platform/Source/Code/*',
    \  3, 'SyPlatform3',
    \ 'C:/SVN/Syandus_ALIVE3/Platform/Source/Code/tags'],
    \
    \ ['C:/SVN/Syandus_ALIVE4/Platform/Source/Code/*',
    \  3, 'SyPlatform4',
    \ 'C:/SVN/Syandus_ALIVE4/Platform/ThirdParty/ogre/Include/tags'],
    \ ['~/SVN/Syandus_ALIVE4/Platform/Source/Code/*',
    \  3, 'SyPlatform4',
    \ ''],
    \ 
    \ 
    \ ['C:/cygwin/home/root/src/ocularwm/*',
    \  4, 'OcularWM',
    \ ''],
    \ 
    \ ['C:/SVN/Syandus_ALIVE3/Tools/Source/SyHandleGen/*',
    \  2, 'SyHandleGen',
    \ ''],
    \ ['C:/SVN/Syandus_ALIVE4/Tools/Source/SyHandleGen/*',
    \  2, 'SyHandleGen',
    \ ''],
    \
    \ ['C:/SVN/Syandus_ALIVE4/Tools/Source/Launcher/*',
    \  3, 'Launcher',
    \ ''],
    \ 
    \ ['C:/SVN/Syandus_ALIVE4/Frameworks/Carbon/*',
    \  3, 'Carbon',
    \ ''],
    \ ['C:/SVN/Syandus_ALIVE4/Frameworks/Oxygen/*',
    \  3, 'Oxygen',
    \ ''],
    \ ['C:/SVN/Syandus_ALIVE4/Frameworks/Proton/*',
    \  3, 'Proton',
    \ ''],
    \
    \ ['C:/SVN/Syandus_Cores/C_ImmunoSim_01/*',
    \  3, 'ImmunoSim',
    \ ''],
    \ 
    \ ['C:/SVN/Syandus_Cores/C_Ogre_Lair_01/*',
    \  4,
    \ 'OgreLair' .
    \       ' C:/SVN/Syandus_Cores/C_Ogre_Lair_01/Source/Scripts/Content/configure.bat',
    \ ''],
    \ 
    \ ]

" in the form of [directory, Console2 name, command to issue]
let s:console2_project_settings = [
    \ ['C:/cygwin/home/root/src/vim/src/*',
    \  'Vim', '_compile.bat{Enter}'],
    \
    \ ['C:/cygwin/home/root/src/vim/src/java/*',
    \  'VimJava', 'nmake{Space}-f{Space}Make_mvc.mak{Enter}'],
    \]

" in the form of [directory, tab width, optional arg, tags]
let s:winscp_project_settings = [
    \ ['C:/SVN/Syandus_ALIVE4/Web/Merck/Phase\ 1/MainMenu/*',
    \  4, '', ''],
    \
    \ ['C:/SVN/Syandus_ALIVE3/Hub/Web/*',
    \  4, '', ''],
    \
    \ ['C:/SVN/Syandus_Company/Web/Syandus.com/main/2012-html/*',
    \  2, '', ''],
    \ ['C:/SVN/Syandus_Company/Web/Syandus.com/main/2013-html/*',
    \  2, '', ''],
    \
    \ ]

for setting in s:visual_studio_project_settings
    let cmd = "au BufNewFile,BufRead,BufEnter " .
        \ setting[0] .
        \  " call SetSettingsForVisualStudioProject(" .
        \ setting[1] . ',"' .
        \ setting[2] . '","' .
        \ setting[3] . '")'
    exe cmd
endfor
for setting in s:console2_project_settings
    let cmd = "au BufNewFile,BufRead,BufEnter " .
        \ setting[0] .
        \  ' call SetConsoleMakeSpaceM("' .
        \ setting[1] . '","' .
        \ setting[2] . '")'
    exe cmd
endfor
for setting in s:winscp_project_settings
    let cmd = "au BufNewFile,BufRead,BufEnter " .
        \ setting[0] .
        \  " call SetSettingsForWinScpProject(" .
        \ setting[1] . ',"' .
        \ setting[2] . '","' .
        \ setting[3] . '")'
    exe cmd
endfor

exe 'augroup END'

" Merck
augroup retroSyrus
    au!
    au BufNewFile,BufRead,BufEnter
        \ C:/SVN/Syandus_ALIVE4/Web/Merck/Phase\ 1/PCRD/retroSyrus/*
        \ call SetSpaceMExternalCmd("C:/SVN/Syandus_ALIVE4/Web/Merck/Phase 1/PCRD/retroSyrus/make.bat")
augroup END

" Shaders
augroup Shaders
    au!
    au BufNewFile,BufRead,BufEnter *.fx call SetSettingsForShaders()
augroup END

augroup ForceClojureIndentation
    au!
    au BufNewFile,BufRead,BufEnter *.clj call SetSettingsForClojure()
    au BufNewFile,BufRead,BufEnter *.cljs call SetSettingsForClojure()
augroup END

augroup FrameworkCarbonLua
    au!
    au BufNewFile,BufRead,BufEnter
        \ C:/SVN/Syandus_ALIVE4/Frameworks/Carbon/Build/Content/Scripts/*.lua
        \ call SetSpaceM('call PropagateCarbonFrameworkLua()')
augroup END

augroup FrameworkOxygenLua
    au!
    au BufNewFile,BufRead,BufEnter
        \ C:/SVN/Syandus_ALIVE4/Frameworks/Oxygen/Build/Content/Scripts/*.lua
        \ call SetSpaceM('call PropagateOxygenFrameworkLua()')
augroup END

augroup CarbonFrameworkOgre
    au!
    au BufNewFile,BufRead,BufEnter
        \ C:/SVN/Syandus_ALIVE4/Frameworks/Carbon/Build/Content/OGRE/*
        \ call SetSpaceM('call PropagateCarbonFrameworkOgre()')
augroup END

augroup OxygenFrameworkOgre
    au!
    au BufNewFile,BufRead,BufEnter
        \ C:/SVN/Syandus_ALIVE4/Frameworks/Oxygen/Build/Content/OGRE/*
        \ call SetSpaceM('call PropagateOxygenFrameworkOgre()')
augroup END

function! AutoConfigureSpaceM()
  let prev_dir = ''
  let current_dir = expand('%:h')

  let max_search = 5

  while current_dir != prev_dir && current_dir != '/'
    let max_search += 1

    let makefile = current_dir . '/Makefile'
    if filereadable(makefile)
      execute "nnoremap <buffer> <leader>m :update<CR>:!make -f Makefile " .
          \ '-C ' . current_dir . '<CR>'
    endif

    let prev_dir = current_dir
    let current_dir = simplify(current_dir . '/..')
    if max_search == 10
      break
    endif
  endwhile

  return
endfunction

