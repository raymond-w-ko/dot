" no tags by default, omegacomplete is usually enough
set tags=

    "\ 'Platform',           '__SVN__/SVN/Syandus_ALIVE3/Platform/Source/Code',
    "\ 'Carbon',             '__SVN__/SVN/Syandus_ALIVE3/Frameworks/Carbon',
    "\ 'CarbonCME',          '__SVN__/SVN/Syandus_ALIVE3/Frameworks/CarbonCME',
    "\ 'Hub',                '__SVN__/SVN/Syandus_ALIVE3/Hub/Source',
    "\ 'Metrics',            '__SVN__/SVN/Syandus_ALIVE3/Metrics',
let s:commands = [
    \ 'Omegacomplete',      '__UNIX_HOME__/dot/.vim/bundle/omegacomplete',
    \ 'Omegacomplete2',     '__UNIX_HOME__/dot/.vim/bundle/omegacomplete2',
    \ 'OcularWM',           '__UNIX_HOME__/src/ocularwm',
    \ 'Windmenu',           '__UNIX_HOME__/src/windmenu',
    \ 'Dk2test',            '__UNIX_HOME__/src/dk2test',
    \ 'Collimator',         '__UNIX_HOME__/src/collimator',
    \ 'Diffractor',         '__UNIX_HOME__/src/diffractor',
    \
    \ 'SVN',                '__SVN__/SVN/',
    \ 'Platform4',          '__SVN__/SVN/Syandus_ALIVE4/Platform/Source/Code',
    \ 'Doc4',               '__SVN__/SVN/Syandus_ALIVE4/Documentation',
    \ 'Carbon4',            '__SVN__/SVN/Syandus_ALIVE4/Frameworks/Carbon',
    \ 'Oxygen',             '__SVN__/SVN/Syandus_ALIVE4/Frameworks/Oxygen',
    \ 'Hydrogen',           '__SVN__/SVN/Syandus_ALIVE4/Frameworks/Hydrogen',
    \ 'Nitrogen',           '__SVN__/SVN/Syandus_ALIVE4/Frameworks/Nitrogen',
    \ 'Proton',             '__SVN__/SVN/Syandus_ALIVE4/Frameworks/Proton',
    \ 'Symlin',             '__SVN__/SVN/Syandus_Cores/C_Sym_DM_01',
    \ 'Spiriva',            '__SVN__/SVN/Syandus_Cores/C_Spv_COPD_01',
    \ 'Copd',               '__SVN__/SVN/Syandus_Cores/C_COPD_Treatment_01',
    \ 'ImmuneQuest',        '__SVN__/SVN/Syandus_Cores/C_ImmunoSim_01',
    \ 'MsPatientEd',        '__SVN__/SVN/Syandus_Cores/C_MS_PatientEd_01',
    \ 'HemoPatientEd',      '__SVN__/SVN/Syandus_Cores/C_Hemo_PatientEd_01',
    \ 'Treatment',          '__SVN__/SVN/Syandus_Cores/C_MS_Treatment_01',
    \ 'MmTreatment',        '__SVN__/SVN/Syandus_Cores/C_MM_Treatment_01',
    \ 'MCRC',               '__SVN__/SVN/Syandus_Cores/C_mCRC_Treatment_01',
    \ 'Sutent',             '__SVN__/SVN/Syandus_Cores/C_Sut_AE_01',
    \ 'SyMetrics',          '__SVN__/SVN/Syandus_ALIVE3/Metrics/SyMetrics',
    \ 'SyLogParser',        '__SVN__/SVN/Syandus_ALIVE3/Metrics/SyLoginParser',
    \ 'SyHandleGen',        '__SVN__/SVN/Syandus_ALIVE3/Tools/Source/SyHandleGen',
    \ 'SyHandleGen4',       '__SVN__/SVN/Syandus_ALIVE4/Tools/Source/SyHandleGen',
    \ 'Groundhog',          '__SVN__/SVN/Syandus_ALIVE3/Groundhog',
    \ 'GroundhogClient',    '__SVN__/SVN/Syandus_ALIVE3/Groundhog/Client',
    \ 'GroundhogServer',    '__SVN__/SVN/Syandus_ALIVE3/Groundhog/Server',
    \ 'GroundhogShared',    '__SVN__/SVN/Syandus_ALIVE3/Groundhog/Shared',
    \ 'ConnectionTester',   '__SVN__/SVN/Syandus_ALIVE3/Groundhog/ConnectionTester',
    \ 'SyRefresh',          '__SVN__/SVN/Syandus_ALIVE3/Tools/Source/SyRefresh',
    \ 'SyProjectGenerator', '__SVN__/SVN/Syandus_ALIVE4/Tools/Source/SyProjectGenerator',
    \ 'OgreLair',           '__SVN__/SVN/Syandus_Cores/C_Ogre_Lair_01',
    \ 'Ms',                 '__SVN__/SVN/Syandus_Cores/C_CMSC_MS_01',
    \ 'SyandusHtml5',       '__SVN__/SVN/Syandus_Company/Web/Syandus.com/main/2013-html/html',
    \ 'Cellulose',          '__SVN__/SVN/Syandus_ALIVE4/Cellulose',
    \ 'Rosettastone',       '__SVN__/SVN/Syandus_Web/Merck/rosettastone',
    \ 'Merck',              '__SVN__/SVN/Syandus_Web/Merck',
    \ ]
for i in range(len(s:commands) / 2)
    let cmd = (i * 2) + 0

    let dir = s:commands[(i * 2) + 1]
    let dir = MyTranslateDirectory(dir)

    let g:my_project_directories[dir] = 1

    exe 'com! ' . s:commands[cmd] . ' cd ' . dir
endfor

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" UNIX
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! FindAndRunMakefile()
  let prev_dir = ''
  let current_dir = expand('%:p:h')

  let max_search = 0

  while current_dir != prev_dir && current_dir != '/'
    let max_search += 1

    let makefile = current_dir . '/Makefile'
    if filereadable(makefile)
      echom current_dir
      execute "!make -f Makefile " . '-C ' . current_dir
      return
    endif

    let prev_dir = current_dir
    let current_dir = simplify(current_dir . '/..')
    if max_search == 8
      break
    endif
  endwhile
endfunction

if has('unix')
  nnoremap <leader>m :update<CR>:call FindAndRunMakefile()<CR>
endif

" vim:fdm=marker:foldlevel=0
