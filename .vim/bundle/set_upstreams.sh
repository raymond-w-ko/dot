#!/bin/bash

set -e

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$DIR"

if [ -d "./Lucius" ]; then
    cd Lucius
    pwd
    if [ -e ".git" ]; then
        git remote add upstream git://github.com/vim-scripts/Lucius.git
    fi
    cd ..
fi

if [ -d "./bufkill.vim" ]; then
    cd bufkill.vim
    pwd
    if [ -e ".git" ]; then
        git remote add upstream git://github.com/vim-scripts/bufkill.vim.git
    fi
    cd ..
fi
