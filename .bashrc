# If not running interactively, don't do anything
[[ "$-" != *i* ]] && return

export HISTSIZE=32768
export HISTCONTROL=ignoreboth:erasedups

export PATH="$HOME/bin:/opt/local/bin:/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/sbin:/opt/aws/bin:/opt/mono/bin"
export TMP='/tmp'
export TEMP='/tmp'

#function parse_git_branch {
    #git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
#}

function prompt_command {
  ACTUAL_LAST_RET=$?

  whiteBold="\[\033[1;37m\]"
  white="\[\033[0;37m\]"
  blueBold="\[\033[1;34m\]"
  blue="\[\033[0;34m\]"
  greenBold="\[\033[1;32m\]"
  green="\[\033[0;32m\]"
  redBold="\[\033[1;31m\]"
  yellowBold="\[\033[1;33m\]"
  yellow="\[\033[0;33m\]"
  cyanBold="\[\033[1;36m\]"
  cyan="\[\033[0;36m\]"
  purpleBold="\[\033[1;35m\]"
  normalColor="\[\033[0m\]"
  #dash="\342\224\200"
  dash="-"

  RET_STATUS="\$(if [[ $ACTUAL_LAST_RET == 0 ]]; then echo -n \"$greenBold\"; echo ret: $ACTUAL_LAST_RET; else echo -n \"$redBold\"; echo ret: $ACTUAL_LAST_RET; fi)"
  function BatteryStatus {
      if hash acpi 2>/dev/null; then
        acpi | sed 's/Battery 0: //' | sed 's/ remaining//'
      else
        echo "no acpi"
      fi
  }
  USER_AT_HOST="$(if [[ ${EUID} == 0 ]]; then echo "$redBold\h"; else echo "$blueBold\u@\h"; fi)"
  BATTERY="\$(BatteryStatus)"
  FILE_INFO="\$(ls -1 | wc -l | sed 's: ::g') files, \$(ls -lah | grep -m 1 total | sed 's/total //')b"
  LINE1="$whiteBold($cyan\D{%Y %b %e %l:%M:%S %p}$whiteBold)$dash($green$BATTERY$whiteBold)"
  LINE2="($yellow$FILE_INFO$whiteBold)$dash($yellow\w$white$whiteBold)"
  LINE3="$whiteBold($USER_AT_HOST$whiteBold)$dash($white$RET_STATUS$whiteBold)"
  PROMPT="$dash> $normalColor"
  export PS1="\n$LINE1\n$LINE2\n$LINE3$PROMPT`echo $REAL_LAST_RET`"
}
export PROMPT_COMMAND=prompt_command

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
