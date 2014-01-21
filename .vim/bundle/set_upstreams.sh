#!/bin/bash

set -e

function set_upstream_git() {
  if [ -d "$1" ]; then
      cd "$1"
      pwd
      if [ -e ".git" ]; then
          set +e
          git remote rm upstream
          git remote add upstream "$2"
          set -e
          git remote -v
      fi
      cd ..
  fi
  echo "--------------------------------------------------------------------------------"
}

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$DIR"

set_upstream_git './Lucius' 'git://github.com/vim-scripts/Lucius.git'
set_upstream_git './seoul256.vim' 'git://github.com/junegunn/seoul256.vim.git'
set_upstream_git './bufkill.vim' 'git://github.com/vim-scripts/bufkill.vim.git'
