# If not running interactively, don't do anything
[[ "$-" != *i* ]] && return

export HISTSIZE=32768
export HISTCONTROL=ignoreboth:erasedups

#export PATH="$HOME/bin:/opt/local/bin:/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/sbin:/usr/bin/core_perl:/opt/aws/bin:/opt/mono/bin:/opt/dropbox"

export CCACHE_SLOPPINESS=time_macros
export PATH="/usr/lib/ccache/bin:$HOME/bin:/opt/local/bin:/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/sbin:/usr/bin/core_perl:/opt/aws/bin:/opt/mono/bin:/opt/dropbox"

export TMP='/tmp'
export TEMP='/tmp'
export PKG_CONFIG_PATH="/usr/lib/pkgconfig/:/usr/local/lib/pkgconfig/"

################################################################################
# PS1 prompt
################################################################################
function parse_git_branch {
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}

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
  purple="\[\033[0;35m\]"
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
  function GitBranch {
    if hash git 2>/dev/null; then
      GIT_BRANCH=`git branch 2>/dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'`
      if [[ ! -z "$GIT_BRANCH" ]]; then
        echo "$GIT_BRANCH"
      else
        echo ""
      fi
    else
      echo ""
    fi
  }
  USER_AT_HOST="$(if [[ ${EUID} == 0 ]]; then echo "$redBold\h"; else echo "$blueBold\u@\h"; fi)"
  BATTERY="\$(BatteryStatus)"
  FILE_INFO="\$(ls -1 2>/dev/null | wc -l | sed 's: ::g') files, \$(ls -lah 2>/dev/null | grep -m 1 total | sed 's/total //')b"
  LINE1="$whiteBold($cyan\D{%Y %b %e %l:%M:%S %p}$whiteBold)$dash($green$BATTERY$whiteBold)"
  LINE2="($yellow$FILE_INFO$whiteBold)$dash$green\$(GitBranch)$whiteBold$dash($yellow\w$white$whiteBold)"
  LINE3="$whiteBold($USER_AT_HOST$whiteBold)$dash($white$RET_STATUS$whiteBold)"
  PROMPT="$dash> $normalColor"
  export PS1="\n$LINE1\n$LINE2\n$LINE3$PROMPT`echo $REAL_LAST_RET`"
}
export PROMPT_COMMAND=prompt_command

unset PYTHONHOME

unameString=`uname -s`
if [[ "$unameString" == 'Darwin' ]]; then
    if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
        alias vim="vim"
    else
        alias vim='mvim'
    fi

    #export DYLD_LIBRARY_PATH=~/boost/stage/lib

    if hash gdircolors 2>/dev/null; then
      if [ -r "$HOME/src/dircolors-solarized/dircolors.256dark" ]; then
        eval `gdircolors $HOME/src/dircolors-solarized/dircolors.256dark`
      fi
    fi

    if hash gls 2>/dev/null; then
      alias ls='gls --color -F'
      alias l='gls --color -Flh'
      alias ll='gls --color -Flha'
      alias ls='gls --color'
    else
      export LSCOLORS=ExFxCxDxBxegedabagacad
      alias ls='ls -FG'
      alias l='ls -FGlh'
      alias ll='ls -FGlha'
    fi


else
    alias ls='ls --color -F'
    alias l='ls --color -Flh'
    alias ll='ls --color -Flha'

    # fix ls colors especially for directories and files that are globally readable and writeable
    if [ -r "$HOME/src/dircolors-solarized/dircolors.256dark" ]; then
      eval `dircolors $HOME/src/dircolors-solarized/dircolors.256dark`
    fi
fi

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'

idleionice() {
  ionice -c3 -p $@
}

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
export EDITOR=vim

# aliases
alias sdr='screen -U -D -R'
alias ta='tmux attach'
alias tmux="TERM=screen-256color-bce tmux"
alias svnlog='svn log -l 128 -v | less'
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
elif [ -d "/cygdrive/c/Program Files/Vim/vim74/src" ]; then
    alias vimsrc='cd "/cygdrive/c/Program Files/Vim/vim74/src"'
elif [ -d "/cygdrive/c/Program Files (x86)/Vim/vim73/src" ]; then
    alias vimsrc='cd "/cygdrive/c/Program Files (x86)/Vim/vim73/src"'
elif [ -d "/cygdrive/c/Program Files (x86)/Vim/vim74/src" ]; then
    alias vimsrc='cd "/cygdrive/c/Program Files (x86)/Vim/vim74/src"'
elif [ -d "$HOME/src/vim" ]; then
    alias vimsrc="cd $HOME/src/vim"
fi

alias makeinstallvim="vimsrc && hpa && cd src && make && rm -r ~/vim/ && make install"

if [ -d "$HOME/android-ndk-r9d" ]
then
  export NDK_HOME=$HOME/android-ndk-r9d
  export ANDROID_NDK=$HOME/android-ndk-r9d
  export PATH="$PATH:$HOME/android-ndk-r9d"
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
alias ga='git add'

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
if hash colordiff 2>/dev/null; then
  svndiff() {
    svn diff -x "-w --ignore-eol-style" "${@}" | sed 's///' | colordiff | less -R
  }
else
  svndiff() {
    svn diff -x "-w --ignore-eol-style" "${@}" | sed 's///' | less -R
  }
fi
alias svnignore='svn propedit svn:ignore'

alias fix_permissions="find . -regex '.*\.\(vim\|h\|hpp\|c\|cpp\)$' -type f -exec chmod -x {} \;"

# custom work aliases
alias Platform4="cd ~/SVN/Syandus_ALIVE4/Platform/Source/Code"
if [[ "$unameString" == 'Darwin' ]]; then
  alias ThirdParty4="cd ~/SVN/Syandus_ALIVE4/Platform/ThirdParty/Mac"
else
  alias ThirdParty4="cd ~/SVN/Syandus_ALIVE4/Platform/ThirdParty/Linux"
fi
alias ImmuneQuest="cd ~/SVN/Syandus_Cores/C_ImmunoSim_01"
alias PatientEducation="cd ~/SVN/Syandus_Cores/C_MS_PatientEd_01"
alias Treatment="cd ~/SVN/Syandus_Cores/C_MS_Treatment_01"
alias Cellulose="cd ~/SVN/Syandus_ALIVE4/Cellulose"
alias Hydrogen="cd ~/SVN/Syandus_ALIVE4/Frameworks/Hydrogen/Build/Content"
alias Oxygen="cd ~/SVN/Syandus_ALIVE4/Frameworks/Oxygen/Build/Content"
alias Nitrogen="cd ~/SVN/Syandus_ALIVE4/Frameworks/Nitrogen/Build/Content"

if [[ "$unameString" == 'Darwin' ]]; then
  alias ImmuneQuestBuilds="cd ~/Desktop/ImmuneQuest_Builds"
fi

if hash stty 2>/dev/null; then
  stty stop undef
  stty start undef
  stty erase 
fi

# TMUX
#if which tmux 2>&1 >/dev/null; then
  ##if not inside a tmux session, and if no session is started, start a new session
  #test -z "$TMUX" && (tmux attach || tmux new-session)
#fi
