#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$DIR"

if [ -d "./Lucius" ]; then
    cd Lucius
    pwd
    git remote add upstream git://github.com/vim-scripts/Lucius.git
    cd ..
fi

if [ -d "./bufkill.vim" ]; then
    cd bufkill.vim
    pwd
    git remote add upstream git://github.com/vim-scripts/bufkill.vim.git
    cd ..
fi
