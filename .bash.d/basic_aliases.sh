unameString=`uname -s`

if [[ "$unameString" == 'Darwin' ]]; then
    if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
        alias vim="vim"
    else
        alias vim='mvim'
    fi

    if hash gdircolors 2>/dev/null; then
      if [ -r "$HOME/src/dircolors-solarized/dircolors.256dark" ]; then
        eval `gdircolors $HOME/src/dircolors-solarized/dircolors.256dark`
      fi
    fi

    if hash gls 2>/dev/null; then
      alias ls='gls --color -F'
      alias l='gls --color -Flh'
      alias lt='gls --color -Flht'
      alias ll='gls --color -Flha'
      alias ls='gls --color'
    else
      export LSCOLORS=ExFxCxDxBxegedabagacad
      alias ls='ls -FG'
      alias l='ls -FGlh'
      alias lt='ls -FGlht'
      alias ll='ls -FGlha'
    fi
else
    alias ls='ls --color'
    alias l='ls --color -lh'
    alias lt='ls --color -lht'
    alias ll='ls --color -lha'

    # fix ls colors especially for directories and files that are globally readable and writeable
    if [ -r "$HOME/src/dircolors-solarized/dircolors.256dark" ]; then
      eval `dircolors $HOME/src/dircolors-solarized/dircolors.256dark`
    fi
fi

alias v=vim

alias ..='cd ..'
alias ..2='cd ../..'
alias ..3='cd ../../..'
alias ..4='cd ../../../..'
alias ..5='cd ../../../../..'
alias ..6='cd ../../../../../..'

# old cygwin wrapper
# vim
#vim() {
  #if [ `expr "$*" : '.*tex\>'` -gt 0 ] ; then
  #  opt='--servername LATEX '
  #fi
  #PYTHONHOME="C:/Python27/App" HOME="C:/Users/root" cyg-wrapper.sh "C:/Program Files (x86)/Vim/vim73/gvim.exe" "$@"
#}
