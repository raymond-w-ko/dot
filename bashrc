# If not running interactively, don't do anything
[[ "$-" != *i* ]] && return

export HISTSIZE=32768
export HISTCONTROL=ignoreboth

export PATH="/opt/local/bin:$HOME/bin:/bin:/usr/local/bin:/usr/bin:/usr/local/sbin:/usr/sbin:/sbin:/opt/aws/bin:/opt/mono/bin"
export TMP='/tmp'
export TEMP='/tmp'

export PS1="\n\[\e[32;1m\](\[\e[37;1m\]\h\[\e[32;1m\])-(\[\e[37;1m\]jobs:\j\[\e[32;1m\])-(\[\e[37;1m\]\w\[\e[32;1m\])\n(\[\[\e[37;1m\]\u\[\e[32;1m\]) \\$ \[\e[0m\]"

unset PYTHONHOME

unamestr=`uname -s`
if [[ "$unamestr" == 'Darwin' ]]; then
    alias ls='ls -FG'
    alias l='ls -FG'
    alias ll='ls -lFG'

    alias vim='mvim'

    export DYLD_LIBRARY_PATH=~/boost/stage/lib
else
    alias ls='ls --color -F'
    alias l='ls --color -F'
    alias ll='ls --color -lF'
fi

alias dot='cd ~/lib/dot'
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

alias v=vim

export SVN_EDITOR=vim
export GIT_EDITOR=vim
export HGEDITOR=vim

# aliases
alias sdr='screen -D -R'
alias svnlog='svn log -l 32 -v | less'
alias myctags='/usr/bin/find . -regex ".*\.\(c\|h\|hpp\|cc\|cpp\)" -print | /usr/bin/ctags --c++-kinds=+px --fields=+aimSz --languages=c++ --sort=yes -L -'
alias omegacomplete='cd ~/lib/dot/vim/bundle/omegacomplete'
alias p2p='cd /cygdrive/c/Users/root/Desktop/P2P'

alias gs='git status'
alias gc='git commit'
alias gca='git commit -a'
alias gd='git diff'
alias gpush='git push'
alias gpull='git pull'

alias hs='hg status'
alias hm='hg merge'
alias hu='hg update'
alias hc='hg commit'
alias hd='hg diff'
alias hpa='hg purge --all'
alias hpush='hg push'
alias hpull='hg pull'

alias fix_permissions="find . -regex '.*\.\(vim\|h\|hpp\|cpp\|cpp\)' -type f -exec chmod -x {} \;"
