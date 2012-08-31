#!/bin/bash
# THIS IS A WORK IN PROGRESS
# BE CAREFUL, DAMMIT

set -e

echo "prerequisites: python git vim"

function ensure_link {
    test -L "$HOME/$2" || ln -s "$HOME/$1" "$HOME/$2"
}

#mkdir -p ~/.config/fish
#mkdir -p ~/lib/hg
#mkdir -p ~/lib/virtualenvs
mkdir -p ~/bin
mkdir -p ~/src


#test -d ~/.hg-git/    || hg clone "bb://durin42/hg-git/" "$HOME/.hg-git"
#test -d ~/lib/dulwich || git clone "git://github.com/jelmer/dulwich.git" "$HOME/lib/dulwich"

#ensure_link "lib/dulwich/dulwich" "lib/hg/hg/dulwich"

#test -d ~/lib/dot || hg clone http://bitbucket.org/sjl/dot ~/lib/dot

ensure_link "lib/dot/minttyrc"       ".minttyrc"

ensure_link "lib/dot/profile"        ".profile"
ensure_link "lib/dot/bash_profile"   ".bash_profile"
ensure_link "lib/dot/bashrc"         ".bashrc"

ensure_link "lib/dot/gitconfig"      ".gitconfig"
ensure_link "lib/dot/gitignore"      ".gitignore"
ensure_link "lib/dot/hgrc"           ".hgrc"

echo completed
exit

ensure_link "lib/dot/tmux/tmux.conf" ".tmux.conf"
ensure_link "lib/dot/vim"            ".vim"
ensure_link "lib/dot/vim/vimrc"      ".vimrc"
ensure_link "lib/dot/ackrc"          ".ackrc"
ensure_link "lib/dot/weechat"        ".weechat"
ensure_link "lib/dot/urlview"        ".urlview"
ensure_link "lib/dot/pentadactylrc"  ".pentadactylrc"
ensure_link "lib/dot/offlineimaprc"  ".offlineimaprc"
ensure_link "lib/dot/mutt"           ".mutt"
ensure_link "lib/dot/dotjs"          ".js"
ensure_link "lib/dot/dotcss"         ".css"
ensure_link "lib/dot/hgignore"       ".hgignore"
ensure_link "lib/dot/ctags"          ".ctags"
ensure_link "lib/dot/grc"            ".grc"
ensure_link "lib/dot/fish/config.fish" ".config/fish/config.fish"
