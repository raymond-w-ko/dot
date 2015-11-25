export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

# If not running interactively, don't do anything
[[ "$-" != *i* ]] && return

## workaround for handling TERM variable in multiple tmux sessions properly from http://sourceforge.net/p/tmux/mailman/message/32751663/ by Nicholas Marriott
if [[ -n ${TMUX} && -n ${commands[tmux]} ]];then
  case $(tmux showenv TERM 2>/dev/null) in
    *256color) ;&
    TERM=fbterm)
      TERM=screen-256color ;;
    *)
      TERM=screen
  esac
fi

if [[ "$SHELL" == bash ]]; then
  # attempts to correct bad "cd" target
  shopt -s cdspell
  shopt -s checkwinsize
  shopt -s cmdhist
  shopt -s expand_aliases
  shopt -s extglob
  shopt -s histappend
  shopt -s autocd 2>/dev/null
fi

export HISTSIZE=32768
export HISTCONTROL=ignoreboth:erasedups

export CCACHE_SLOPPINESS=pch_defines,time_macros
if [[ $(uname -s) == CYGWIN* ]]; then
  export PATH="$HOME/vim/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/lib/lapack"
else
  export PATH="/home/rko/npm-global/bin:$HOME/vim/bin:/usr/lib/ccache/bin:/opt/local/bin:/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/sbin:/usr/bin/core_perl:/opt/aws/bin:/opt/mono/bin:/opt/dropbox:$PATH"
fi

. ~/.bash.d/add_dot_extra_paths.sh

[[ -z "$TMP" ]] && export TMP='/tmp'
[[ -z "$TEMP" ]] && export TEMP='/tmp'
export PKG_CONFIG_PATH="/usr/lib/pkgconfig/:/usr/local/lib/pkgconfig/"

unameString=`uname -s`

if [[ "$SHELL" == bash ]]; then
  . ~/.bash.d/ps1.sh
fi

unset PYTHONHOME

. ~/.bash.d/basic_aliases.sh
. ~/.bash.d/colored_man_pages.sh

idleionice() {
  ionice -c3 -p $@
}

export SVN_EDITOR=vim
export GIT_EDITOR=vim
export HGEDITOR=vim
export EDITOR=vim

if hash colordiff 2>/dev/null; then
  DIFF_PROG=colordiff
else
  DIFF_PROG=diff
fi

hash schedtool 2>/dev/null
if [[ $? == 0 && -f /proc/sys/kernel/rr_interval ]]; then
	NICE_PROG="schedtool -D -e"
else
  NICE_PROG="nice -n 19"
fi

# aliases
alias dot="cd $HOME/dot"
alias sdr='screen -U -D -R'
# covered by oh-my-zsh, but I do not want to type argument
alias ta='tmux attach'
alias genctags='/usr/bin/find . -regex ".*\.\(c\|h\|hpp\|cc\|cpp\)" -print | /usr/bin/ctags --c++-kinds=+px --fields=+aimSz --languages=c++ --sort=yes -L -'
alias omegacomplete='cd ~/.vim/bundle/omegacomplete'
alias killpngcolorpofile='find . -type f -name "*.png" -exec convert {} -strip {} \;'
alias iotop='sudo iotop -oP'
alias iftop='sudo iftop'
alias openports='ss --all --numeric --processes --ipv4 --ipv6'
alias n19='nice -n 19'
alias n="$NICE_PROG"
alias makepkg="$NICE_PROG makepkg"
alias yaourt="$NICE_PROG yaourt"
alias yup="$NICE_PROG yaourt -Syua --noconfirm"
alias y="yaourt"
findcore() {
  find . -type f -regextype posix-extended -regex '.*/core\.[0-9]+$'
}
alias uu='udevil umount'
alias us='update-submodules'
alias fix_permissions="find . -regex '.*\.\(vim\|h\|hpp\|c\|cpp\)$' -type f -exec chmod -x {} \;"
superwget() {
  while [ 1 ]; do
    wget --retry-connrefused --waitretry=1 --read-timeout=20 --timeout=15 -t 0 --continue "$1"
    if [ $? = 0 ]; then
      break;
    fi; # check return value, break if successful (0)
    sleep 1s;
  done;
}

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

alias makeinstallvim="vimsrc && git clean -fxd && cd src && $NICE_PROG make && rm -rf ~/vim/ && make install"

if [ -d $HOME/android-ndk-r10d ]
then
  export NDK_HOME=$HOME/android-ndk-r10d
  export ANDROID_NDK=$HOME/android-ndk-r10d
  export PATH="$PATH:$HOME/android-ndk-r10d"
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
alias gcsubmodules='git commit -m "updated submodule(s)"'
alias gd='git diff'
alias gpush='git push'
alias gpull='git pull'
alias gpum='git pull upstream master'
alias ga='git add'
alias gcfxd='git clean -fxd'

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

alias svnlog='svn log -l 1024 -v | less'
alias svnfixexe="find . -name '*.sh' -exec svn propset svn:executable yes '{}' \;"
svnrmmissing() {
  svn st | grep ^! | awk '{print " --force "$2}' | xargs svn rm
}
svnaddmissing() {
  svn st | grep '^?' | awk '{print " --force "$2}' | xargs svn add
}
alias svnadddir='svn add --depth=empty'
svndiff() {
  svn diff -x "-w --ignore-eol-style" "${@}" | sed 's///' | $DIFF_PROG | less -R
}
svnshow() {
  svn diff -x "-w --ignore-eol-style" -c $1 | sed 's///' | $DIFF_PROG | less -R
}
alias svnignore='svn propedit svn:ignore'

# custom work aliases
alias Platform4="cd ~/SVN/Syandus_ALIVE4/Platform/Source/Code"
alias Platform5="cd ~/SVN/Syandus_ALIVE5"
if [[ "$unameString" == 'Darwin' ]]; then
  alias ThirdParty4="cd ~/SVN/Syandus_ALIVE4/Platform/ThirdParty/Mac"
else
  alias ThirdParty4="cd ~/SVN/Syandus_ALIVE4/Platform/ThirdParty/Linux"
  alias ThirdParty4Android="cd ~/SVN/Syandus_ALIVE4/Platform/ThirdParty/Android"
fi
alias ImmuneQuest="cd ~/SVN/Syandus_Cores/C_ImmunoSim_01"
alias mspatienteducation="cd ~/SVN/Syandus_Cores/C_MS_PatientEd_01"
alias hemopatiented="cd ~/SVN/Syandus_Cores/C_Hemo_PatientEd_01"
alias Treatment="cd ~/SVN/Syandus_Cores/C_MS_Treatment_01"
alias mmtreatment="cd ~/SVN/Syandus_Cores/C_MM_Treatment_01"
alias mcrc="cd ~/SVN/Syandus_Cores/C_mCRC_Treatment_01"
alias copd="cd ~/SVN/Syandus_Cores/C_COPD_Treatment_01"
alias Cellulose="cd ~/SVN/Syandus_ALIVE4/Cellulose"
alias Proton="cd ~/SVN/Syandus_ALIVE4/Frameworks/Proton/Build/Content"
alias Hydrogen="cd ~/SVN/Syandus_ALIVE4/Frameworks/Hydrogen/Build/Content"
alias Oxygen="cd ~/SVN/Syandus_ALIVE4/Frameworks/Oxygen/Build/Content"
alias Nitrogen="cd ~/SVN/Syandus_ALIVE4/Frameworks/Nitrogen/Build/Content"
alias merck="cd ~/SVN/Syandus_Web/Merck"

if [[ "$unameString" == 'Darwin' ]]; then
  alias ImmuneQuestBuilds="cd ~/Desktop/ImmuneQuest_Builds"
fi

if hash stty 2>/dev/null; then
  stty stop undef
  stty start undef
  # breaks backspace in hidden password reading prompts
  # actually only if xterm backspace is set to generate ^H and not ^?
  stty erase 
fi

ulimit -c unlimited
#export MC_SKIN=$HOME/.config/mc/solarized.ini
. ~/dot/bin/base16-solarized.dark.sh

# TMUX
#if which tmux 2>&1 >/dev/null; then
  ##if not inside a tmux session, and if no session is started, start a new session
  #test -z "$TMUX" && (tmux attach || tmux new-session)
#fi

if [[ -f /usr/bin/virtualenvwrapper.sh ]]; then
  source /usr/bin/virtualenvwrapper.sh
fi

if [[ -z $DISPLAY && $(tty) == /dev/tty1 ]]; then
  # we can't completely redirect stderr to a file otherwise root-less X breaks
  # I'm guessing it is determining the VTTY from the stderr file descriptor
  #startx | tee > "$HOME/.xsession-errors"
  # maybe this works from the archlinux wiki?
  exec startx -- -keeptty -nolisten tcp > "$HOME/.xsession-errors" 2>&1
  logout
elif [[ -f ~/src/interkonnect/interkonnect.py && $(ps auxww | grep interkonnect.py | grep -v grep | wc -l) = 0 ]]; then
  sudo ~/src/interkonnect/interkonnect.py
else
  if [[ "$unameString" != 'Darwin' && `uname -o` != "Cygwin" ]]; then
    if hash gnome-keyring-daemon 2>/dev/null; then
      export DISPLAY=:0
      if test -z "$DBUS_SESSION_BUS_ADDRESS" ; then
        ## if not found, launch a new one
        #eval `dbus-launch --sh-syntax`
        export DBUS_SESSION_BUS_ADDRESS="autolaunch:"
      fi
      if [[ -z $SSH_AUTH_SOCK ]]; then
        eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
        export GPG_AGENT_INFO
        export SSH_AUTH_SOCK
      fi
    fi
  fi
  if hash fortune 2>/dev/null; then
    if hash cowsay 2>/dev/null; then
      fortune | cowsay -W 70 2>/dev/null
    else
      fortune
    fi
  fi
fi
