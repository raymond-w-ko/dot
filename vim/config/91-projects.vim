" no tags by default, omegacomplete is usually enough
set tags=

let s:project_directories = [
    \ 'Dropbox',            'C:/Users/__USERNAME__/Desktop/Dropbox',
    \ 'OmegaComplete',      'C:/cygwin/home/rko/lib/dot/vim/bundle/omegacomplete',
    \ 'Vim',                'C:/cygwin/home/rko/src/vim',
    \ 'OcularWM',           'C:/cygwin/home/rko/src/ocularwm',
    \
    \ 'Platform',           'C:/SVN/Syandus_ALIVE3/Platform/Source/Code',
    \ 'Platform4',          'C:/SVN/Syandus_ALIVE4/Platform/Source/Code',
    \ 'Doc4',               'C:/SVN/Syandus_ALIVE4/Documentation',
    \ 'Carbon',             'C:/SVN/Syandus_ALIVE3/Frameworks/Carbon',
    \ 'CarbonCME',          'C:/SVN/Syandus_ALIVE3/Frameworks/CarbonCME',
    \ 'Carbon4',            'C:/SVN/Syandus_ALIVE4/Frameworks/Carbon',
    \ 'Oxygen',             'C:/SVN/Syandus_ALIVE4/Frameworks/Oxygen',
    \ 'Hub',                'C:/SVN/Syandus_ALIVE3/Hub/Source',
    \ 'Metrics',            'C:/SVN/Syandus_ALIVE3/Metrics',
    \ 'Symlin',             'C:/SVN/Syandus_Cores/C_Sym_DM_01',
    \ 'Spiriva',            'C:/SVN/Syandus_Cores/C_Spv_COPD_01',
    \ 'Copd',               'C:/SVN/Syandus_Cores/C_Unb_COPD_01',
    \ 'Immunobiology',      'C:/SVN/Syandus_Cores/C_ImmunoSim_01',
    \ 'Sutent',             'C:/SVN/Syandus_Cores/C_Sut_AE_01',
    \ 'SyMetrics',          'C:/SVN/Syandus_ALIVE3/Metrics/SyMetrics',
    \ 'SyLogParser',        'C:/SVN/Syandus_ALIVE3/Metrics/SyLoginParser',
    \ 'SyHandleGen',        'C:/SVN/Syandus_ALIVE3/Tools/Source/SyHandleGen',
    \ 'SyHandleGen4',       'C:/SVN/Syandus_ALIVE4/Tools/Source/SyHandleGen',
    \ 'Groundhog',          'C:/SVN/Syandus_ALIVE3/Groundhog',
    \ 'GroundhogClient',    'C:/SVN/Syandus_ALIVE3/Groundhog/Client',
    \ 'GroundhogServer',    'C:/SVN/Syandus_ALIVE3/Groundhog/Server',
    \ 'GroundhogShared',    'C:/SVN/Syandus_ALIVE3/Groundhog/Shared',
    \ 'ConnectionTester',   'C:/SVN/Syandus_ALIVE3/Groundhog/ConnectionTester',
    \ 'SyRefresh',          'C:/SVN/Syandus_ALIVE3/Tools/Source/SyRefresh',
    \ 'SyProjectGenerator', 'C:/SVN/Syandus_ALIVE4/Tools/Source/SyProjectGenerator',
    \ 'OgreLair',           'C:/SVN/Syandus_Cores/C_Ogre_Lair_01',
    \ 'Ms',                 'C:/SVN/Syandus_Cores/C_CMSC_MS_01',
    \ 'SyandusHtml5',       'C:/SVN/Syandus_Company/Web/Syandus.com/main/2013-html/html',
    \ ]
let s:username = expand('$USERNAME')
for i in range(len(s:project_directories) / 2)
    let cmd = (i * 2) + 0
    let dir = s:project_directories[(i * 2) + 1]
    let dir = substitute(dir, '__USERNAME__', s:username, '')
    let dir = substitute(dir, ' ', '\ ', '')
    if s:project_directories[cmd] == 'Dropbox'
        if !isdirectory(dir)
            let dir = substitute(dir, '/Desktop', '', '')
        endif
    endif
    exe 'com! ' . s:project_directories[cmd] . ' cd ' . dir
endfor

function! SetSettingsForVisualStudioProject(size_of_tab, name, tags, ...)
    if (a:size_of_tab > 0)
        execute 'setlocal tabstop=' . a:size_of_tab
        execute 'setlocal shiftwidth=' . a:size_of_tab 
        execute 'setlocal softtabstop=' . a:size_of_tab
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
	execute 'setlocal tabstop=' . a:size_of_tab
	execute 'setlocal shiftwidth=' . a:size_of_tab 
	execute 'setlocal softtabstop=' . a:size_of_tab
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
    setlocal tabstop=4 shiftwidth=4 softtabstop=4
    nnoremap <buffer> <leader>m :update<CR>:!start .\install.bat<CR>
    setlocal tags=
endfunction
function! SetSettingsForClojure()
    setlocal tabstop=2 shiftwidth=2 softtabstop=2
    setlocal tags=
endfunction

exe 'augroup MyProjectSettings'
exe 'au!'

" follows format of [directory, tab width, VS project name, tags list]
let s:visual_studio_project_settings = [
    \ ['C:/cygwin/home/root/lib/dot/vim/bundle/omegacomplete/*',
    \  4, 'OmegaCompletePythonModule', ''],
    \ 
    \
    \ ['C:/SVN/Syandus_ALIVE3/Platform/Source/Code/*',
    \  3, 'SyPlatform3',
    \ 'C:/SVN/Syandus_ALIVE3/Platform/Source/Code/tags'],
    \
    \ ['C:/SVN/Syandus_ALIVE4/Platform/Source/Code/*',
    \  3, 'SyPlatform4',
    \ 'C:/SVN/Syandus_ALIVE4/Platform/ThirdParty/ogre/Include/tags'],
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

" vim:fdm=marker:foldlevel=0
