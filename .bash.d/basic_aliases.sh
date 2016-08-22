unameString=`uname -s`

if [[ "$unameString" == 'Darwin' ]]; then
    if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
        alias vim="vim"
    else
        alias vim='mvim'
    fi

    # if hash gdircolors 2>/dev/null; then
    #   if [ -r "$HOME/src/dircolors-solarized/dircolors.256dark" ]; then
    #     eval `gdircolors $HOME/src/dircolors-solarized/dircolors.256dark`
    #   fi
    # fi

    if hash gls 2>/dev/null; then
      alias ls='gls --color'
      alias l='gls --color -lh'
      alias lt='gls --color -lht'
      alias ll='gls --color -lha'
      alias ls='gls --color'
    else
      export LSCOLORS=ExFxCxDxBxegedabagacad
      alias ls='ls -G'
      alias l='ls -Glh'
      alias lt='ls -Glht'
      alias ll='ls -Glha'
    fi
else
    alias ls='ls --color=never -F'
    alias l='ls --color=never -lhF'
    alias lt='ls --color=never -lhtF'
    alias ll='ls --color=never -lhaF'
    
    export GREP_COLOR='1;32'

    # fix ls colors especially for directories and files that are globally
    # readable and writeable, they are completely unreadable by default
    # if [ -r "$HOME/src/dircolors-solarized/dircolors.256dark" ]; then
    #   eval `dircolors $HOME/src/dircolors-solarized/dircolors.256dark`
    # else
    #   eval `dircolors -b`
    # fi
fi

alias v=vim
alias vr="vim -R"
alias vs="vim -S"

alias ..='cd ..'
alias ..2='cd ../..'
alias ..3='cd ../../..'
alias ..4='cd ../../../..'
alias ..5='cd ../../../../..'
alias ..6='cd ../../../../../..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias ......='cd ../../../../..'
alias .......='cd ../../../../../..'

# old cygwin wrapper
# vim
#vim() {
  #if [ `expr "$*" : '.*tex\>'` -gt 0 ] ; then
  #  opt='--servername LATEX '
  #fi
  #PYTHONHOME="C:/Python27/App" HOME="C:/Users/root" cyg-wrapper.sh "C:/Program Files (x86)/Vim/vim73/gvim.exe" "$@"
#}
