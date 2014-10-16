# If not running interactively, don't do anything
[[ "$-" != *i* ]] && return

# attempts to correct bad "cd" target
shopt -s cdspell
shopt -s checkwinsize
shopt -s cmdhist
shopt -s expand_aliases
shopt -s extglob
shopt -s histappend
shopt -s autocd 2>/dev/null

export HISTSIZE=32768
export HISTCONTROL=ignoreboth:erasedups

export CCACHE_SLOPPINESS=time_macros
export PATH="/usr/lib/ccache/bin:/opt/local/bin:/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/sbin:/usr/bin/core_perl:/opt/aws/bin:/opt/mono/bin:/opt/dropbox:$PATH"

. ~/.bash.d/add_dot_extra_paths.sh

[[ -z "$TMP" ]] && export TMP='/tmp'
[[ -z "$TEMP" ]] && export TEMP='/tmp'
export PKG_CONFIG_PATH="/usr/lib/pkgconfig/:/usr/local/lib/pkgconfig/"

unameString=`uname -s`

. ~/.bash.d/ps1.sh

unset PYTHONHOME

. ~/.bash.d/basic_aliases.sh

idleionice() {
  ionice -c3 -p $@
}

export SVN_EDITOR=vim
export GIT_EDITOR=vim
export HGEDITOR=vim
export EDITOR=vim

# aliases
alias sdr='screen -U -D -R'
alias ta='tmux attach'
alias tmux="TERM=screen-256color-bce tmux"
alias svnlog='svn log -l 1024 -v | less'
alias svnfixexe="find . -name '*.sh' -exec svn propset svn:executable yes '{}' \;"
svnrmmissing() {
  svn st | grep ^! | awk '{print " --force "$2}' | xargs svn rm
}
alias genctags='/usr/bin/find . -regex ".*\.\(c\|h\|hpp\|cc\|cpp\)" -print | /usr/bin/ctags --c++-kinds=+px --fields=+aimSz --languages=c++ --sort=yes -L -'
alias omegacomplete='cd ~/lib/dot/vim/bundle/omegacomplete'
alias killpngcolorpofile='find . -type f -name "*.png" -exec convert {} -strip {} \;'
alias iotop='sudo iotop -oP'
alias iftop='sudo iftop'
alias openports='ss --all --numeric --processes --ipv4 --ipv6'
alias n19='nice -n 19'
alias n='nice -n 19'
alias makepkg='nice -n 19 makepkg'
alias yaourt='nice -n 19 yaourt'
alias yup='nice -n 19 yaourt -Syua --noconfirm'
findcore() {
  find . -type f -regextype posix-extended -regex '.*/core\.[0-9]+$'
}
alias uu='udevil umount'

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

alias makeinstallvim="vimsrc && hpa && cd src && nice -n 19 make && rm -rf ~/vim/ && make install"

if [ -d "$HOME/android-ndk-r10b" ]
then
  export NDK_HOME=$HOME/android-ndk-r10b
  export ANDROID_NDK=$HOME/android-ndk-r10b
  export PATH="$PATH:$HOME/android-ndk-r10b"
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

alias us='update-submodules'

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
alias mcrc="cd ~/SVN/Syandus_Cores/C_mCRC_Treatment_01"
alias copd="cd ~/SVN/Syandus_Cores/C_COPD_Treatment_01"
alias Cellulose="cd ~/SVN/Syandus_ALIVE4/Cellulose"
alias Proton="cd ~/SVN/Syandus_ALIVE4/Frameworks/Proton/Build/Content"
alias Hydrogen="cd ~/SVN/Syandus_ALIVE4/Frameworks/Hydrogen/Build/Content"
alias Oxygen="cd ~/SVN/Syandus_ALIVE4/Frameworks/Oxygen/Build/Content"
alias Nitrogen="cd ~/SVN/Syandus_ALIVE4/Frameworks/Nitrogen/Build/Content"

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
export MC_SKIN=$HOME/.config/mc/solarized.ini

# TMUX
#if which tmux 2>&1 >/dev/null; then
  ##if not inside a tmux session, and if no session is started, start a new session
  #test -z "$TMUX" && (tmux attach || tmux new-session)
#fi

if [[ ! $DISPLAY && $(tty) = /dev/tty1 ]]; then
  # we can't completely redirect stderr to a file otherwise root-less X breaks
  # I'm guessing it is determining the VTTY from the stderr file descriptor
  startx 2>&1 | tee > "$HOME/.xsession-errors"
  logout
elif [[ -f ~/src/interkonnect/interkonnect.py && $(ps auxww | grep interkonnect.py | grep -v grep | wc -l) = 0 ]]; then
  sudo ~/src/interkonnect/interkonnect.py
else
  if hash fortune 2>/dev/null; then
    if hash cowsay 2>/dev/null; then
      fortune | cowsay 2>/dev/null
    else
      fortune
    fi
  fi
fi
