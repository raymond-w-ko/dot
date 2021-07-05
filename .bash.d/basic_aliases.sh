unameString=`uname -s`

if [[ "$unameString" == 'Darwin' ]]; then
    # if hash gdircolors 2>/dev/null; then
    #   if [ -r "$HOME/src/dircolors-solarized/dircolors.256dark" ]; then
    #     eval `gdircolors $HOME/src/dircolors-solarized/dircolors.256dark`
    #   fi
    # fi

    if hash gls 2>/dev/null; then
      alias ls='gls -F'
      alias l='gls -lhF'
      alias lt='gls -lhtF'
      alias ll='gls -lhaF'
    else
      export LSCOLORS=ExFxCxDxBxegedabagacad
      alias ls='ls -G'
      alias l='ls -Glh'
      alias lt='ls -Glht'
      alias ll='ls -Glha'
    fi
else
    alias ls='ls --color=auto -F'
    alias l='ls --color=auto -lhF'
    alias lt='ls --color=auto -lhtF'
    alias ll='ls --color=auto -lhaF'
    
    export GREP_COLOR='1;32'

    # fix ls colors especially for directories and files that are globally
    # readable and writeable, they are completely unreadable by default
    if [ -r "$HOME/src/dircolors-solarized/dircolors.256dark" ]; then
      # eval `dircolors $HOME/src/dircolors-solarized/dircolors.256dark`
      eval `dircolors $HOME/src/dircolors-solarized/dircolors.ansi-dark`
      export LS_COLORS="$LS_COLORS:ow=1;7;34:st=30;44:su=30;41"
    else
      eval `dircolors -b`
    fi
fi

if [[ -d $HOME/nvim ]]; then
  alias v=nvim
  alias vS='nvim -S'
elif hash nvim 2>/dev/null; then
  alias v=nvim
  alias vS='nvim -S'
else
  alias v=vim
  alias vS='vim -S'
fi
alias rmr="rm -r"
alias rmri="rm -r -i"
alias r="ranger"
alias sr="sudo ranger"

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
