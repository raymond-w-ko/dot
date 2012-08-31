# If not running interactively, don't do anything
[[ "$-" != *i* ]] && return

export HISTSIZE=32768
export HISTCONTROL=ignoreboth

export PATH="$HOME/bin:/usr/local/bin:/usr/bin:/usr/local/sbin:/usr/sbin:/sbin:/opt/aws/bin:/opt/mono/bin"
export TMP='/tmp'
export TEMP='/tmp'

export PS1="\n\[\e[32;1m\](\[\e[37;1m\]\h\[\e[32;1m\])-(\[\e[37;1m\]jobs:\j\[\e[32;1m\])-(\[\e[37;1m\]\w\[\e[32;1m\])\n(\[\[\e[37;1m\]\u\[\e[32;1m\]) \\$ \[\e[0m\]"

unset PYTHONHOME

alias ls='ls --color -F'
alias l='ls --color -F'
alias ll='ls --color -lF'

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'

# vim
#vim() {
  #if [ `expr "$*" : '.*tex\>'` -gt 0 ] ; then
  #  opt='--servername LATEX '
  #fi
  #PYTHONHOME="C:/Python27/App" HOME="C:/Users/root" cyg-wrapper.sh "C:/Program Files (x86)/Vim/vim73/gvim.exe" "$@"
#}

#export SVN_EDITOR='PYTHONHOME="C:/Python27/App" HOME="C:\Users\root" cyg-wrapper.sh "C:/Program Files (x86)/Vim/vim73/gvim.exe"'
#export GIT_EDITOR='PYTHONHOME="C:/Python27/App" HOME="C:\Users\root" cyg-wrapper.sh "C:/Program Files (x86)/Vim/vim73/gvim.exe"'
#export HGEDITOR='PYTHONHOME="C:/Python27/App" HOME="C:\Users\root" cyg-wrapper.sh "C:/Program Files (x86)/Vim/vim73/gvim.exe"'

# aliases
alias svnlog="svn log -l 32 -v | less"

alias gs='git status'
alias gc='git commit'
alias gca='git commit -a'
alias gd='git diff'

alias hs='hg status'
alias hm='hg merge'
alias hu='hg update'
alias hc='hg commit'
alias hd='hg diff'
alias hpa='hg purge --all'
