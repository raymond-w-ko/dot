# If not running interactively, don't do anything
[[ "$-" != *i* ]] && return

export HISTSIZE=32768
export HISTCONTROL=ignoreboth:erasedups

export PATH="$HOME/bin:/opt/local/bin:/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/sbin:/opt/aws/bin:/opt/mono/bin"
export TMP='/tmp'
export TEMP='/tmp'

function parse_git_branch {
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}

#export PS1="\n\[\e[32;1m\](\[\e[37;1m\]\h\[\e[32;1m\])-(\[\e[37;1m\]jobs:\j\[\e[32;1m\])-\$(parse_git_branch)-(\[\e[37;1m\]\w\[\e[32;1m\])\n(\[\e[37;1m\]\u\[\e[32;1m\]) \\$ \[\e[0m\]"

# http://maketecheasier.com/8-useful-and-interesting-bash-prompts/2009/09/04
white="\[\033[1;37m\]"
blue="\[\033[1;34m\]"
green="\[\033[1;32m\]"
red="\[\033[01;31m\]"
#dash="\342\224\200"
dash="-"
USER_AT_HOST="$(if [[ ${EUID} == 0 ]]; then echo "$red\h"; else echo "$blue\u@\h"; fi)"
RET_STATUS="ret: \$(if [[ \$? == 0 ]]; then echo \"$green\$?\"; else echo \"$red\$?\"; fi)"
function BatteryStatus {
    if hash acpi 2>/dev/null; then
      acpi | sed 's/Battery 0: //' | sed 's/ remaining//'
    else
      echo "no acpi"
    fi
}
BATTERY="\$(BatteryStatus)"
FILE_INFO="\$(ls -1 | wc -l | sed 's: ::g') files, \$(ls -lah | grep -m 1 total | sed 's/total //')b"
export PS1="\n$white\342\224\214($USER_AT_HOST$white)$dash($RET_STATUS$white)$dash($blue\@ \d$white)$dash($BATTERY$white)\n\342\224\224$dash($green\w$white)$dash($green$FILE_INFO$white)$dash> \[\033[0m\]"

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

    #export DYLD_LIBRARY_PATH=~/boost/stage/lib
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
  export ANDROID_NDK=$HOME/android-ndk-r9
  export PATH="$PATH:$HOME/android-ndk-r9"
fi

if [ -d "$HOME/android-sdk-linux" ]
then
  export ANDROID_SDK=$HOME/android-sdk-linux
fi

if [ -d "$HOME/android-sdk-linux" ]
then
  export PATH="$PATH:$HOME/android-sdk-linux/tools:$HOME/android-sdk-linux/platform-tools"
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
alias hcmergedwithupstream='hg commit -m "merged with upstream"'

alias svnadddir='svn add --depth=empty'
svndiff() {
  svn diff "${@}" | colordiff | less -R
}

alias fix_permissions="find . -regex '.*\.\(vim\|h\|hpp\|c\|cpp\)$' -type f -exec chmod -x {} \;"

# custom work aliases
alias Platform4="cd ~/SVN/Syandus_ALIVE4/Platform"
alias Immunology="cd ~/SVN/Syandus_Cores/C_ImmunoSim_01"
