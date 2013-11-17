#!/bin/bash
set -e

function ensure_link {
    test -L "$HOME/$2" || ln -s "$HOME/$1" "$HOME/$2"
}

cd "$HOME"
mkdir -p ~/src

if [[ `uname` == 'Darwin' ]]; then
mkdir -p ~/Library/Vim/backup
    mkdir -p ~/Library/Vim/swap
    mkdir -p ~/Library/Vim/undo
else
mkdir -p ~/.local/share/vim/backup
    mkdir -p ~/.local/share/vim/swap
    mkdir -p ~/.local/share/vim/undo
fi

ensure_link "vim/bin/ex" "bin/ex"
ensure_link "vim/bin/rview" "bin/rview"
ensure_link "vim/bin/rvim" "bin/rvim"
ensure_link "vim/bin/vim" "bin/vim"
ensure_link "vim/bin/vimdiff" "bin/vimdiff"
ensure_link "vim/bin/vimtutor" "bin/vimtutor"
ensure_link "vim/bin/xxd" "bin/xxd"
