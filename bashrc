# If not running interactively, don't do anything
[[ "$-" != *i* ]] && return

export HISTSIZE=32768
export HISTCONTROL=ignoreboth

export PATH="$HOME/bin:/opt/local/bin:/bin:/usr/local/bin:/usr/bin:/usr/local/sbin:/usr/sbin:/sbin:/opt/aws/bin:/opt/mono/bin"
export TMP='/tmp'
export TEMP='/tmp'

function parse_git_branch {
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}

export PS1="\n\[\e[32;1m\](\[\e[37;1m\]\h\[\e[32;1m\])-(\[\e[37;1m\]jobs:\j\[\e[32;1m\])-\$(parse_git_branch)-(\[\e[37;1m\]\w\[\e[32;1m\])\n(\[\e[37;1m\]\u\[\e[32;1m\]) \\$ \[\e[0m\]"

unset PYTHONHOME

unamestr=`uname -s`
if [[ "$unamestr" == 'Darwin' ]]; then
    export LSCOLORS=GxFxCxDxBxegedabagaced

    alias ls='ls -FG'
    alias l='ls -FGlh'
    alias ll='ls -FGlha'

    if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
        alias vim="vim"
    else
        alias vim='mvim'
    fi

    export DYLD_LIBRARY_PATH=~/boost/stage/lib

    alias Platform4="cd ~/SVN/Syandus_ALIVE4/Platform"
    alias Immunobiology="cd ~/SVN/Syandus_Cores/C_ImmunoSim_01"
else
    alias ls='ls --color -F'
    alias l='ls --color -Flh'
    alias ll='ls --color -Flha'
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
alias sdr='screen -U -D -R'
alias svnlog='svn log -l 32 -v | less'
alias genctags='/usr/bin/find . -regex ".*\.\(c\|h\|hpp\|cc\|cpp\)" -print | /usr/bin/ctags --c++-kinds=+px --fields=+aimSz --languages=c++ --sort=yes -L -'
alias omegacomplete='cd ~/lib/dot/vim/bundle/omegacomplete'

if [ -d "/cygdrive/c/Users/root/Desktop/P2P" ]; then
    alias p2p='cd /cygdrive/c/Users/root/Desktop/P2P'
fi
if [ -d "/cygdrive/c/Users/Raymond W. Ko/Desktop/P2P" ]; then
    alias p2p='cd "/cygdrive/c/Users/Raymond W. Ko/Desktop/P2P"'
fi

if [ -d "/cygdrive/c/Program Files/Vim/vim73/src" ]; then
    alias vimsrc='cd "/cygdrive/c/Program Files/Vim/vim73/src"'
elif [ -d "/cygdrive/c/Program Files (x86)/Vim/vim73/src" ]; then
    alias vimsrc='cd "/cygdrive/c/Program Files (x86)/Vim/vim73/src"'
elif [ -d "$HOME/src/vim" ]; then
    alias vimsrc="cd $HOME/src/vim"
fi

if [ -d "$HOME/android-ndk-r9" ]
then
  export NDK_HOME=$HOME/android-ndk-r9
fi

alias gs='git status'
alias gc='git commit'
alias gca='git commit -a'
alias gcasubmodules='git commit -a -m "updated submodule(s)"'
alias gd='git diff'
alias gpush='git push'
alias gpull='git pull'
alias gpum='git pull upstream master'

alias hs='hg status'
alias hm='hg merge'
alias hu='hg update'
alias hc='hg commit'
alias hd='hg diff'
alias hdw='hg diff -w'
alias hpa='hg purge --all'
alias hpush='hg push'
alias hpull='hg pull'
alias hcmergewithupstream='hg commit -m "merged with upstream"'

alias fix_permissions="find . -regex '.*\.\(vim\|h\|hpp\|c\|cpp\)$' -type f -exec chmod -x {} \;"
