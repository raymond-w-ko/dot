source /etc/profile
export PS1='profile $ '
PS1='profile $ '

if [ -n "BASH_VERSION" ]
then
  if [ -f "$HOME/.bashrc" ]; then
    source "$HOME/.bashrc"
  fi
fi
