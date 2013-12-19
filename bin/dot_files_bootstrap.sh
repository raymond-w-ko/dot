#!/bin/bash
set -e

if [[ -z "$HOME" ]]; then
  echo '$HOME not set'
  exit 1
fi

if [[ -z "$1" ]]; then
  echo "dot_files_bootstrap.sh machine_id"
  exit 1
fi

function ensure_link {
  rm -f "$HOME/$2"
  ln -s "$HOME/$1" "$HOME/$2"
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

# VIM
ensure_link "vim/bin/ex" "bin/ex"
ensure_link "vim/bin/rview" "bin/rview"
ensure_link "vim/bin/rvim" "bin/rvim"
ensure_link "vim/bin/vim" "bin/vim"
ensure_link "vim/bin/vimdiff" "bin/vimdiff"
ensure_link "vim/bin/vimtutor" "bin/vimtutor"
ensure_link "vim/bin/xxd" "bin/xxd"

# machine specific symbolic links
ensure_link ".xinitrc.$1" ".xinitrc.extra"
ensure_link ".i3status.conf.$1" ".i3status.conf"
ensure_link ".gtkrc-2.0.$1" ".gtkrc-2.0" 
