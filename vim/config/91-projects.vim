set tags=

com! Dropbox cd C:/Users/root/Desktop/Dropbox
com! OmegaComplete cd C:/cygwin/home/root/lib/dot/vim/bundle/omegacomplete
com! Vim cd C:/cygwin/home/root/src/vim

com! Platform cd C:/SVN/Syandus_ALIVE3/Platform/Source/Code
com! Platform4 cd C:/SVN/Syandus_ALIVE4/Platform/Source/Code

com! Carbon cd C:/SVN/Syandus_ALIVE3/Frameworks/Carbon
com! CarbonCME cd C:/SVN/Syandus_ALIVE3/Frameworks/CarbonCME
com! Carbon4 cd C:/SVN/Syandus_ALIVE4/Frameworks/Carbon
com! Oxygen cd C:/SVN/Syandus_ALIVE3/Frameworks/Oxygen

com! Hub cd C:/SVN/Syandus_ALIVE3/Hub/Source
com! Metrics cd C:/SVN/Syandus_ALIVE3/Metrics

com! Symlin cd C:/SVN/Syandus_Cores/C_Sym_DM_01
com! Spiriva cd C:/SVN/Syandus_Cores/C_Spv_COPD_01
com! Copd cd C:/SVN/Syandus_Cores/C_Unb_COPD_01
com! Immunobiology cd C:/SVN/Syandus_Cores/C_ImmunoSim_01
com! Sutent cd C:/SVN/Syandus_Cores/C_Sut_AE_01
com! SyMetrics cd C:/SVN/Syandus_ALIVE3/Metrics/SyMetrics
com! SyLogParser cd C:/SVN/Syandus_ALIVE3/Metrics/SyLoginParser
com! SyHandleGen cd C:/SVN/Syandus_ALIVE3/Tools/Source/SyHandleGen
com! SyHandleGen4 cd C:/SVN/Syandus_ALIVE4/Tools/Source/SyHandleGen
com! Groundhog cd C:/SVN/Syandus_ALIVE3/Groundhog
com! GroundhogClient cd C:/SVN/Syandus_ALIVE3/Groundhog/Client
com! GroundhogServer cd C:/SVN/Syandus_ALIVE3/Groundhog/Server
com! GroundhogShared cd C:/SVN/Syandus_ALIVE3/Groundhog/Shared
com! ConnectionTester cd C:/SVN/Syandus_ALIVE3/Groundhog/ConnectionTester
com! SyRefresh cd C:/SVN/Syandus_ALIVE3/Tools/Source/SyRefresh
com! SyProjectGenerator cd C:/SVN/Syandus_ALIVE4/Tools/Source/SyProjectGenerator
com! OgreLair cd C:/SVN/Syandus_Cores/C_Ogre_Lair_01
com! Ms cd C:/SVN/Syandus_Cores/C_CMSC_MS_01

function! SetSettingsForProject(size_of_tab, arg0, tags, ...)
	execute 'setlocal tabstop=' . a:size_of_tab
	execute 'setlocal shiftwidth=' . a:size_of_tab 
	execute 'setlocal softtabstop=' . a:size_of_tab
    if (len(a:arg0) == 0)
        execute "nnoremap <buffer> <leader>m <nop>"
    else
        execute "nnoremap <buffer> <leader>m :call AutoHotkeyMake('" .
               \ a:arg0 . "')\<CR>"
    endif
	execute 'setlocal tags=' . a:tags
    if a:0 > 0
        setlocal noexpandtab
    endif
endfunction

function! SetSettingsForProject2(size_of_tab, arg0, tags)
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
    execute "nnoremap <silent><buffer> <leader>m :w!<CR>:update<CR>:!start " .
                \ a:cmd .
                \ "<CR>"
endfunction

" personal projects
" OmegaComplete {{{
augroup OmegaCompletePythonModule
    au!
    au BufNewFile,BufRead,BufEnter
    \ C:/cygwin/home/root/lib/dot/vim/bundle/omegacomplete/*
    \ call SetSettingsForProject(
        \ 4,
        \ 'OmegaCompletePythonModule',
        \ '')
augroup END
" }}}
augroup VimSourceCode
    au!
    au BufNewFile,BufRead,BufEnter
    \ C:/cygwin/home/root/src/vim/src/*
    \ call SetConsoleMakeSpaceM('Vim', '_compile.bat{Enter}')
augroup END

augroup VimJava
    au!
    au BufNewFile,BufRead,BufEnter
    \ C:/cygwin/home/root/src/vim/src/java/*
    \ call SetConsoleMakeSpaceM('VimJava', 'make.bat{Enter}')
augroup END


" platform level
" OGRE {{{
augroup OgreSDK
    autocmd!
    autocmd BufNewFile,BufRead,BufEnter
    \ C:/OgreSDK/*
    \ call SetSettingsForProject(
        \ 4,
        \ '',
        \ 'C:/OgreSDK/OgreSDK_vc9_v1-8-0/include/tags')
augroup END

augroup Ogre
    autocmd!
    autocmd BufNewFile,BufRead,BufEnter
    \ C:/SVN/Syandus_ALIVE4/Evaluations/OGRE/*
    \ call SetSettingsForProject(
        \ 4,
        \ 'C:/Users/root/Desktop/Dropbox/make_ogre_application.ahk',
        \ 'C:/OgreSDK/OgreSDK_vc9_v1-8-0/include/tags')
augroup END
"}}}
" Groundhog {{{
augroup Groundhog
    autocmd!
    autocmd BufNewFile,BufRead,BufEnter
    \ C:/SVN/Syandus_ALIVE3/Groundhog/*
    \ call SetSettingsForProject(
        \ 3,
        \ 'C:/Users/root/Desktop/Dropbox/make_groundhog.ahk',
        \ '')
augroup END
augroup ConnectionTester
    autocmd!
    autocmd BufNewFile,BufRead,BufEnter
    \ C:/SVN/Syandus_ALIVE3/Groundhog/ConnectionTester/*
    \ call SetSettingsForProject(
        \ 3,
        \ 'C:/Users/root/Desktop/Dropbox/make_connection_tester.ahk',
        \ '')
augroup END
" }}}
" Platform {{{
augroup Platform3
    autocmd!
    autocmd BufNewFile,BufRead,BufEnter
    \ C:/SVN/Syandus_ALIVE3/Platform/Source/Code/*
    \ call SetSettingsForProject(
        \ 3,
        \ 'SyPlatform3',
        \ 'C:/SVN/Syandus_ALIVE3/Platform/Source/Code/tags')
augroup END
augroup Platform4
    autocmd!
    autocmd BufNewFile,BufRead,BufEnter
    \ C:/SVN/Syandus_ALIVE4/Platform/Source/Code/*
    \ call SetSettingsForProject(
        \ 3,
        \ 'SyPlatform4',
        \ 'C:/SVN/Syandus_ALIVE4/Platform/ThirdParty/OGRE/Include/tags'
        \ )
augroup END
" }}}

" backend, metrics, launcher, misc
" Hub {{{
augroup Hub
    autocmd!
    autocmd BufNewFile,BufRead,BufEnter
    \ C:/SVN/Syandus_ALIVE3/Hub/*
    \ call SetSettingsForProject(
        \ 3,
        \ 'C:/Users/root/Desktop/Dropbox/make_hub.ahk',
        \ 'C:/SVN/Syandus_ALIVE3/Hub/Source/tags')
augroup END
" }}}
" HubWeb {{{
augroup HubWeb
    autocmd!
    autocmd BufNewFile,BufRead,BufEnter
    \ C:/SVN/Syandus_ALIVE3/Hub/Web/*
    \ call SetSettingsForProject2(
        \ 4,
        \ '',
        \ '')
augroup END
" }}}
" SyMetrics {{{
augroup SyMetrics
    autocmd!
    autocmd BufNewFile,BufRead,BufEnter
    \ C:/SVN/Syandus_ALIVE3/Metrics/SyMetrics/*
    \ call SetSettingsForProject(
        \ 2,
        \ 'SyMetrics',
        \ '')
augroup END
" }}}
" SyMetricsWeb {{{
augroup WebMetrics
    autocmd!
    autocmd BufNewFile,BufRead,BufEnter
    \ C:/SVN/Syandus_ALIVE3/Metrics/web/*
    \ call SetSettingsForProject(
        \ 4,
        \ 'UNUSED C:/Users/root/Desktop/Dropbox/winscp_sync.ahk',
        \ '')
augroup END
" }}}
" SyLogParser {{{
augroup SyLoginParser
    autocmd!
    autocmd BufNewFile,BufRead,BufEnter
    \ C:/SVN/Syandus_ALIVE3/Metrics/SyLoginParser/*
    \ call SetSettingsForProject(
        \ 2,
        \ 'C:/Users/root/Desktop/Dropbox/make_sylogparser.ahk',
        \ '')
augroup END
"}}}
" SyHandleGen {{{
augroup SyHandleGen
    autocmd!
    autocmd BufNewFile,BufRead,BufEnter
    \ C:/SVN/Syandus_ALIVE3/Tools/Source/SyHandleGen/*
    \ call SetSettingsForProject(
        \ 2,
        \ 'SyHandleGen',
        \ '')
augroup END
augroup SyHandleGen4
    autocmd!
    autocmd BufNewFile,BufRead,BufEnter
    \ C:/SVN/Syandus_ALIVE4/Tools/Source/SyHandleGen/*
    \ call SetSettingsForProject(
        \ 2,
        \ 'SyHandleGen',
        \ '')
augroup END
"}}}
" SyRefresh {{{
augroup SyRefresh
    autocmd!
    autocmd BufNewFile,BufRead,BufEnter
    \ C:/SVN/Syandus_ALIVE3/Tools/Source/SyRefresh/*
    \ call SetSettingsForProject(
        \ 4,
        \ 'C:/Users/root/Desktop/Dropbox/make_syrefresh.ahk',
        \ '')
augroup END
"}}}
" SyProjectGenerator {{{
augroup SyProjectGenerator
    autocmd!
    autocmd BufNewFile,BufRead,BufEnter
    \ C:/SVN/Syandus_ALIVE4/Tools/Source/SyProjectGenerator/*
    \ call SetSettingsForProject(
        \ 3,
        \ 'C:/Users/root/Desktop/Dropbox/make_syprojectgenerator.ahk',
        \ '')
augroup END
"}}}
" Mac {{{
augroup Mac
    autocmd!
    autocmd BufNewFile,BufRead,BufEnter
    \ /Users/rko/SVN/Syandus_ALIVE3/Mac/trunk/ALIVE\ Med/*
    \ call SetSettingsForProject(
        \ 2,
        \ '',
        \ '')
augroup END
" }}}
" SyMetricsWeb {{{
augroup SyandusDotComHtml5
    autocmd!
    autocmd BufNewFile,BufRead,BufEnter
                \ C:/SVN/Syandus_Company/Web/Syandus.com/main/2012-html/*
                \ call SetSettingsForProject(
                \ 2,
                \ '',
                \ '',
                \ "noexpandtab")
augroup END
" }}}
" Merck Main Menu {{{
augroup MerckMainMenu
    autocmd!
    autocmd BufNewFile,BufRead,BufEnter
                \ C:/SVN/Syandus_ALIVE4/Web/Merck/Phase\ 1/MainMenu/*
                \ call SetSettingsForProject2(
                \ 4,
                \ '',
                \ '')
augroup END

" framework level
" Carbon {{{
augroup Carbon
    autocmd!
    autocmd BufNewFile,BufRead,BufEnter
    \ C:/SVN/Syandus_ALIVE3/Frameworks/Carbon/*
    \ call SetSettingsForProject(
        \ 3,
        \ 'Carbon',
        \ 'C:/SVN/Syandus_ALIVE3/Frameworks/Carbon/Source/Scripts/tags,' .
        \ 'C:/SVN/Syandus_ALIVE3/Platform/SDK/Include/tags')
augroup END
augroup CarbonCME
    autocmd!
    autocmd BufNewFile,BufRead,BufEnter
    \ C:/SVN/Syandus_ALIVE3/Frameworks/CarbonCME/*
    \ call SetSettingsForProject(
        \ 3,
        \ 'CarbonCME',
        \ 'C:/SVN/Syandus_ALIVE3/Frameworks/Carbon/Source/Scripts/tags,' .
        \ 'C:/SVN/Syandus_ALIVE3/Platform/SDK/Include/tags')
augroup END
augroup Carbon4
    autocmd!
    autocmd BufNewFile,BufRead,BufEnter
    \ C:/SVN/Syandus_ALIVE4/Frameworks/Carbon/*
    \ call SetSettingsForProject(
        \ 3,
        \ 'Carbon',
        \ '')
augroup END
" }}}
" Oxygen {{{
augroup Oxygen
    autocmd BufNewFile,BufRead,BufEnter
    \ C:/SVN/Syandus_ALIVE3/Frameworks/Oxygen/*
    \ call SetSettingsForProject(
        \ 3,
        \ 'C:/Users/root/Desktop/Dropbox/make_carbon.ahk',
        \ 'C:/SVN/Syandus_ALIVE3/Frameworks/Oxygen/Source/Scripts/tags,' .
        \ 'C:/SVN/Syandus_ALIVE3/Platform/SDK/Include/tags')
augroup END
" }}}

" simulation cores
" UnbrandedSpiriva {{{
augroup UnbrandedSpiriva
    autocmd!
    autocmd BufNewFile,BufRead,BufEnter
    \ C:/SVN/Syandus_Cores/C_Unb_COPD_01/*
    \ call SetSettingsForProject(
        \ 3,
        \ 'C:/Users/root/Desktop/Dropbox/make_unbrandedcopd.ahk',
        \ 'C:/SVN/Syandus_ALIVE3/Frameworks/Carbon/Source/Scripts/tags,' .
        \ 'C:/SVN/Syandus_ALIVE3/Platform/SDK/Include/tags'
        \ )
augroup END
" }}}
" Symlin {{{
augroup Symlin
    autocmd!
    autocmd BufNewFile,BufRead,BufEnter
    \ C:/SVN/Syandus_Cores/C_Sym_DM_01/*
    \ call SetSettingsForProject(
        \ 2,
        \ 'C:/Users/root/Desktop/Dropbox/make_symlin.ahk',
        \ 'C:/SVN/Syandus_Cores/C_Sym_DM_01/Source/Scripts/Content/tags,' .
        \ 'C:/SVN/Syandus_ALIVE3/Frameworks/Carbon/Source/Scripts/tags,' .
        \ 'C:/SVN/Syandus_ALIVE3/Platform/SDK/Include/tags'
        \ )
augroup END
" }}}
" Sutent {{{
augroup Sutent
    autocmd!
    autocmd BufNewFile,BufRead,BufEnter
    \ C:/SVN/Syandus_Cores/C_Sut_AE_01/*
    \ call SetSettingsForProject(
        \ 3,
        \ 'C:/Users/root/Desktop/Dropbox/make_sutent.ahk',
        \ 'C:/SVN/Syandus_Cores/C_Sut_AE_01/Source/Scripts/Content/tags,' .
        \ 'C:/SVN/Syandus_ALIVE3/Frameworks/Carbon/Source/Scripts/tags,' .
        \ 'C:/SVN/Syandus_ALIVE3/Platform/SDK/Include/tags'
        \ )
augroup END
" }}}
" OgreLair {{{
augroup OgreLair
    autocmd!
    autocmd BufNewFile,BufRead,BufEnter
    \ C:/SVN/Syandus_Cores/C_Ogre_Lair_01/*
    \ call SetSettingsForProject(
        \ 3,
        \ 'OgreLair ' .
        \  'C:/SVN/Syandus_Cores/C_Ogre_Lair_01/Source/Scripts/Content/configure.bat',
        \ ''
        \ )
augroup END
" }}}
" MS {{{
augroup CMSC_MS
    autocmd!
    autocmd BufNewFile,BufRead,BufEnter
    \ C:/SVN/Syandus_Cores/C_CMSC_MS_01/*
    \ call SetSettingsForProject(
        \ 3,
        \ 'CMSC ' .
        \ 'C:/SVN/Syandus_Cores/C_CMSC_MS_01/Source/Scripts/Content/configure.bat',
        \ 'C:/SVN/Syandus_ALIVE3/Frameworks/CarbonCME/Source/Scripts/tags,' .
        \ 'C:/SVN/Syandus_ALIVE3/Platform/SDK/Include/tags'
        \ )
augroup END
" }}}

" immunobiology
" ImmunoSim {{{
augroup ImmunoSim
    autocmd!
    autocmd BufNewFile,BufRead,BufEnter
    \ C:/SVN/Syandus_Cores/C_ImmunoSim_01/*
    \ call SetSettingsForProject(
        \ 3,
        \ 'C:/Users/root/Desktop/Dropbox/make_immunosim.ahk',
        \ 'C:/SVN/Syandus_Cores/C_ImmunoSim_01/Source/Scripts/Content/tags,' .
        \ 'C:/SVN/Syandus_ALIVE3/Frameworks/Oxygen/Source/Scripts/tags,' .
        \ 'C:/SVN/Syandus_ALIVE3/Platform/SDK/Include/tags'
        \ )
augroup END
" }}}

" Merck
augroup retroSyrus
    au!
    autocmd BufNewFile,BufRead,BufEnter
                \ C:/SVN/Syandus_ALIVE4/Web/Merck/Phase\ 1/PCRD/retroSyrus/*
                \ call SetSpaceM( "C:/SVN/Syandus_ALIVE4/Web/Merck/Phase 1/PCRD/retroSyrus/make.bat")
augroup END

" Shaders
function! SetSettingsForShaders()
    setlocal tabstop=4 shiftwidth=4 softtabstop=4
    nnoremap <buffer> <leader>m :update<CR>:!start .\install.bat<CR>
    setlocal tags=
endfunction
augroup Shaders
    autocmd!
    autocmd BufNewFile,BufRead,BufEnter *.fx call SetSettingsForShaders()
augroup END

function! SetSettingsForClojure()
    setlocal tabstop=2 shiftwidth=2 softtabstop=2
    setlocal tags=
endfunction
augroup ForceClojureIndentation
    au!
    autocmd BufNewFile,BufRead,BufEnter *.clj call SetSettingsForClojure()
    autocmd BufNewFile,BufRead,BufEnter *.cljs call SetSettingsForClojure()
augroup END

" vim:fdm=marker:foldlevel=0
