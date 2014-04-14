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
ensure_link ".gtkrc-2.0.mine.$1" ".gtkrc-2.0.mine"
ensure_link ".xbindkeysrc.$1" ".xbindkeysrc"
ensure_link "bin/mymonitors.sh.$1" "bin/mymonitors.sh"
ensure_link "bin/configure_i3_workspaces.sh.$1" "bin/configure_i3_workspaces.sh"

mkdir -p ~/.ssh
chmod 700 ~/.ssh

mkdir -p ~/.vnc
chmod 700 ~/.vnc
