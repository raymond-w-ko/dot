" Since I keep my projects in the UNIX-ish HOME directory, we have to figure
" out where it is. The problem is that it is potentially different everywhere.
if has('win32')
  let dir = 'C:/cygwin/home/root'
  if isdirectory(dir) | let s:unix_home = dir | endif
  let dir = 'C:/cygwin64/home/root'

  if isdirectory(dir) | let s:unix_home = dir | endif
  let dir = 'C:/cygwin/home/Raymond W. Ko'
  if isdirectory(dir) | let s:unix_home = dir | endif
  let dir = 'C:/cygwin64/home/Raymond W. Ko'
  if isdirectory(dir) | let s:unix_home = dir | endif

  let dir = 'C:/cygwin/home/rko'
  if isdirectory(dir) | let s:unix_home = dir | endif
  let dir = 'C:/cygwin64/home/rko'
  if isdirectory(dir) | let s:unix_home = dir | endif
elseif has('win32unix')
  let s:unix_home = expand('$HOME')
else
  let s:unix_home = expand('$HOME')
endif

" Figure out where the company SVN directory lives
if has('win32')
  let s:svn_home = 'C:'
elseif has('win32unix')
  let s:svn_home = '/cygdrive/c'
else
  let s:svn_home = expand('$HOME')
endif

function! MyTranslateDirectory(dir)
  let dir = a:dir
  let dir = substitute(dir, '__SVN__', s:svn_home, '')
  let dir = substitute(dir, '__UNIX_HOME__', s:unix_home, '')
  let dir = substitute(dir, ' ', '\ ', '')

  if has('win32')
    let dir = substitute(dir, '/', '\\', 'g')
  endif

  return dir
endfunction

let s:project_directories_list = [
    \ '__UNIX_HOME__/src/ocularwm',
    \ '__UNIX_HOME__/src/windmenu',
    \ '__UNIX_HOME__/src/dk2test',
    \ '__UNIX_HOME__/src/tsukuyomi',
    \ '__UNIX_HOME__/src/vim/src',
    \ '__UNIX_HOME__/.vim/bundle/omegacomplete',
    \ '__UNIX_HOME__/dot/.vim/bundle/omegacomplete',
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
    \ '__SVN__/SVN/Syandus_ALIVE4/Frameworks/Hydrogen',
    \ '__SVN__/SVN/Syandus_ALIVE4/Frameworks/Nitrogen',
    \ '__SVN__/SVN/Syandus_ALIVE4/Frameworks/Nitrogen16',
    \ '__SVN__/SVN/Syandus_ALIVE4/Platform/Source/Code',
    \ '__SVN__/SVN/Syandus_ALIVE4/Tools/Source/SyProjectGenerator',
    \ '__SVN__/SVN/Syandus_ALIVE4/Tools/Source/mercky',
    \ '__SVN__/SVN/Syandus_ALIVE4/Web/Merck/Phase 1/PCRD/retroSyrus',
    \ '__SVN__/SVN/Syandus_Company/Web/Syandus.com/main/2012-html',
    \ '__SVN__/SVN/Syandus_Company/Web/Syandus.com/main/2013-html/html',
    \ '__SVN__/SVN/Syandus_Cores/C_CMSC_MS_01',
    \ '__SVN__/SVN/Syandus_Cores/C_ImmunoSim_01',
    \ '__SVN__/SVN/Syandus_Cores/C_MS_PatientEd_01',
    \ '__SVN__/SVN/Syandus_Cores/C_MS_Treatment_01',
    \ '__SVN__/SVN/Syandus_Cores/C_mCRC_Treatment_01',
    \ '__SVN__/SVN/Syandus_Cores/C_Mic_HTN_01',
    \ '__SVN__/SVN/Syandus_Cores/C_Ogre_Lair_01',
    \ '__SVN__/SVN/Syandus_Cores/C_Spv_COPD_01',
    \ '__SVN__/SVN/Syandus_Cores/C_Sut_AE_01',
    \ '__SVN__/SVN/Syandus_Cores/C_Sym_DM_01',
    \ '__SVN__/SVN/Syandus_Cores/C_Unb_COPD_01',
    \ '__SVN__/SVN/Syandus_ALIVE4/Cellulose',
    \ ]

let g:my_project_directories = {}
for directory in s:project_directories_list
  let dir = MyTranslateDirectory(directory)
  let g:my_project_directories[dir] = 1
endfor

" traverse up parent directories until it finds one that matches in the above
" list
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
    %:s/$//e
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
