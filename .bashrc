export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
# If not running interactively, don't do anything
[[ "$-" != *i* ]] && return

# if hash zsh 2>/dev/null; then
#   if [[ -n $BASH ]]; then
#     exec zsh
#   fi
# fi

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

export GPG_TTY=$(tty)
# export CCACHE_SLOPPINESS=pch_defines,time_macros

appendpath () {
    case ":$PATH:" in
        *:"$1":*)
            ;;
        *)
            PATH="${PATH:+$PATH:}$1"
    esac
}
prependpath () {
    case ":$PATH:" in
        *:"$1":*)
            ;;
        *)
            PATH="$1${PATH:+:$PATH}"
    esac
}
if [[ $(uname -s) == CYGWIN* ]]; then
  prependpath "/usr/lib/lapack"
  prependpath "/sbin"
  prependpath "/usr/sbin"
  prependpath "/bin"
  prependpath "/usr/bin"
  prependpath "/usr/local/bin"
  prependpath "$HOME/bin"
  prependpath "$HOME/vim/bin"
  prependpath "$HOME/nvim/bin"
else
  prependpath "$HOME/src/neil"
  prependpath "/opt/mono/bin"
  prependpath "/opt/aws/bin"
  prependpath "/usr/bin/core_perl"
  prependpath "/sbin"
  prependpath "/usr/sbin"
  prependpath "/usr/local/sbin"
  prependpath "/bin"
  prependpath "/usr/bin"
  prependpath "/usr/local/bin"
  prependpath "/opt/local/bin"
  if [[ -d  "/usr/lib/ccache/bin" ]]; then
    prependpath "/usr/lib/ccache/bin" 
  else
    prependpath "/usr/lib/ccache" 
  fi
  prependpath "/usr/local/opt/node@8/bin"
  prependpath "/opt/tastyworks"
  prependpath "$HOME/npm-global/bin"
  prependpath "$HOME/.config/yarn/global/node_modules/.bin"
  prependpath "$HOME/.local/bin"
  prependpath "$HOME/vim/bin"
  prependpath "$HOME/nvim/bin"
  prependpath "$HOME/go/bin"
  prependpath "$HOME/dot/bin"
  prependpath "$HOME/bin"
fi
unset appendpath
unset prependpath

. ~/.bash.d/add_dot_extra_paths.sh

[[ -z "$TMP" ]] && export TMP='/tmp'
[[ -z "$TEMP" ]] && export TEMP='/tmp'
# DON'T DO THIS! breaks neovim and probably other compilation
# export PKG_CONFIG_PATH="/usr/lib/pkgconfig/:/usr/local/lib/pkgconfig/"

unameString=`uname -s`

if [[ "$SHELL" == bash ]]; then
  . ~/.bash.d/ps1.sh
fi

unset PYTHONHOME

. ~/.bash.d/basic.sh

idleionice() {
  ionice -c3 -p $@
}

if [[ -d $HOME/nvim ]]; then
  export SVN_EDITOR=nvim
  export GIT_EDITOR=nvim
  export HGEDITOR=nvim
  export EDITOR=nvim
elif hash nvim 2>/dev/null; then
  export SVN_EDITOR=nvim
  export GIT_EDITOR=nvim
  export HGEDITOR=nvim
  export EDITOR=nvim
else
  export SVN_EDITOR=vim
  export GIT_EDITOR=vim
  export HGEDITOR=vim
  export EDITOR=vim
fi

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

if [[ -x $(command -v fzf) ]]; then
  alias f="fzf"
fi
export BAT_THEME=gruvbox-light
if [[ -x $(command -v fdfind) ]]; then
  export FZF_DEFAULT_OPTS="--no-bold"
  export FZF_DEFAULT_COMMAND='fdfind --type f --hidden --exclude .git --exclude .fzf --exclude .svn'
elif [[ -x $(command -v fd) ]]; then
  export FZF_DEFAULT_OPTS="--no-bold"
  export FZF_DEFAULT_COMMAND='fd --type f --hidden --exclude .git --exclude .fzf --exclude .svn'
fi
# fe [FUZZY PATTERN] - Open the selected file with the default editor
#   - Bypass fuzzy finder if there's only one match (--select-1)
#   - Exit if there's no match (--exit-0)
fe() {
  local files
  IFS=$'\n' files=($(fzf-tmux --query="$1" --multi --select-1 --exit-0))
  [[ -n "$files" ]] && ${EDITOR:-vim} "${files[@]}"
}

# aliases
alias dot="cd $HOME/dot"
alias SS="sudo systemctl"
alias sdr='screen -U -D -R'
# covered by oh-my-zsh, but I do not want to type argument
alias ta='tmux attach -d'
alias psref="gpg-connect-agent RELOADAGENT /bye" # refresh gpg
alias genctags='/usr/bin/find . -regex ".*\.\(c\|h\|hpp\|cc\|cpp\)" -print | /usr/bin/ctags --c++-kinds=+px --fields=+aimSz --languages=c++ --sort=yes -L -'
alias killpngcolorpofile='find . -type f -name "*.png" -exec convert {} -strip {} \;'
alias iotop='sudo iotop -oP'
alias iftop='sudo iftop'
alias openports='ss --all --numeric --processes --ipv4 --ipv6'
alias n19='nice -n 19'
alias n="$NICE_PROG"
alias makepkg="$NICE_PROG makepkg"
alias y="$NICE_PROG yay"
alias adu="sudo apt-get update && sudo apt-get dist-upgrade"
alias fun="fusermount -u"
alias npmreinstall="rm -rf node_modules/ && npm install"

if [[ -f "/usr/bin/backblaze-b2" ]]; then
  alias bbl=/usr/bin/backblaze-b2
elif [[ -f "$HOME/.local/bin/b2" ]]; then
  alias bbl="$HOME/.local/bin/b2"
elif [[ -f "/usr/local/bin/b2" ]]; then
  alias bbl="/usr/local/bin/b2"
fi

findcore() {
  find . -type f -regextype posix-extended -regex '.*/core\.[0-9]+$'
}
alias uu='udevil umount'
alias us='update-submodules'
alias rg="rg -i --colors 'match:bg:yellow' --colors 'match:fg:black' --colors 'match:style:nobold' --colors 'line:fg:yellow' --colors 'path:fg:green'"
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

if [ -d "$HOME/src/vim" ]; then
    alias vimsrc="cd $HOME/src/vim"
fi
if [ -d "$HOME/src/neovim" ]; then
    alias nvimsrc="cd $HOME/src/neovim"
fi

alias makeinstallvim="vimsrc && git clean -fxd && cd src && $NICE_PROG make && rm -rf ~/vim/ && make install && cd .."

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

alias gs='git status' # override ghostscript
# alias gc='git commit -v'
# alias gca='git commit -v -a'
alias gcasubmodules='git commit -v -a -m "updated submodule(s)"'
alias gcsubmodules='git commit -v -m "updated submodule(s)"'
# alias gd='git diff'
# alias gdw='git diff --color-words'
# alias gp='git push'
# alias gl='git pull'
# alias ga='git add'
# alias gaa='git add --all'
# alias gapa='git add --patch'
alias gcfxd='git clean -fxd'
# alias gcfxdrh='git clean -fxd && git reset --hard'
alias gitrmmissing='git ls-files --deleted -z | xargs -0 git rm'
alias gdch="git diff --color-words='\+|.'"

# alias hs='hg status'
# alias hm='hg merge'
# alias hu='hg update'
# alias hc='hg commit'
# alias hd='hg diff'
# alias hdw='hg diff -w'
# alias hpa='hg purge --all'
# alias hpush='hg push'
# alias hpull='hg pull'
# alias hcmergedwithupstream='hg commit -m "merged with upstream"'

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
  svn diff --diff-cmd colordiff -x "-u -w -p" "$@" | less -R
}
svnshow() {
  svn diff --diff-cmd colordiff -x "-u -w -p" -c $1 | less -R
}
alias svnignore='svn propedit svn:ignore'

# docker LOL
dockerclean() {
  docker rm -v $(docker ps --filter status=exited -q 2>/dev/null) 2>/dev/null
  docker rmi $(docker images --filter dangling=true -q 2>/dev/null) 2>/dev/null
}
dockercleanstoppedimages() {
  docker ps -aq --no-trunc | xargs docker rm
}

# custom work aliases
alias omegacomplete='cd ~/.vim/bundle/omegacomplete'
alias Platform4="cd ~/SVN/Syandus_ALIVE4/Platform/Source/Code"
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
alias Proton4="cd ~/SVN/Syandus_ALIVE4/Frameworks/Proton/Build/Content"
alias Hydrogen="cd ~/SVN/Syandus_ALIVE4/Frameworks/Hydrogen/Build/Content"
alias Oxygen="cd ~/SVN/Syandus_ALIVE4/Frameworks/Oxygen/Build/Content"
alias merck="cd ~/src/merck"
alias Sydocs="cd ~/src/sydocs"
alias Alive5="cd ~/src/alive5"
alias Proton="cd ~/src/alive5/frameworks/proton"
alias Alive5server="cd ~/src/alive5-server"
alias Nitrogen="cd ~/src/alive5/frameworks/nitrogen"
alias Fluorine="cd ~/src/alive5/frameworks/fluorine"
alias Platform="cd ~/src/alive5/syplatform"
alias Diabetes="cd ~/src/alive5/apps/diabetes_cmesim_2015"
alias Amddr="cd ~/src/alive5/apps/dr_amd_cmesim_2016"
alias Hiv="cd ~/src/alive5/apps/hiv_cmesim_2016"
alias LungGain="cd ~/src/alive5/apps/lung_gain_2016"
alias Obesity="cd ~/src/alive5/apps/obesity_cmesim_2016"
alias Gibleed="cd ~/src/alive5/apps/gi_bleed_2016"
alias Nsclc="cd ~/src/alive5/apps/nsclc_cmesim_2017"
alias Portal="cd ~/src/alive5/portal"
alias Parsenip="cd ~/src/alive5/tools/parsenip"

if [[ "$unameString" == 'Darwin' ]]; then
  alias ImmuneQuestBuilds="cd ~/Desktop/ImmuneQuest_Builds"
fi

sbscd() {
  echo "$@"
  remote_path=$(echo "$@" | tr '\\' '/' | sed 's/\/192.168.1.3\/Art/\/mnt\/art/g')
  cd "$remote_path"
}

stty -ixon # disable ctrl-s and ctrl-q
if hash stty 2>/dev/null; then
  stty stop undef
  stty start undef
  # breaks backspace in hidden password reading prompts
  # actually only if xterm backspace is set to generate ^H and not ^?
  stty erase 
fi

ulimit -c unlimited
#export MC_SKIN=$HOME/.config/mc/solarized.ini
#. ~/dot/bin/base16-solarized.dark.sh

if [[ -n "$DISPLAY" ]]; then
  export GTK_IM_MODULE=ibus
  export XMODIFIERS=@im=ibus
  export QT_IM_MODULE=ibus
fi

if [[ -f ~/.bashrc.local ]]; then
  source ~/.bashrc.local
fi

if [[ ! -d /mnt/c/Windows && -z $DISPLAY && $(tty) == /dev/tty1 ]]; then
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
      # don't force set diplay, otherwise tmux might set the wrong TERM
      # export DISPLAY=:0
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
  if [[ "$unameString" == 'Darwin' ]]; then
    if hash docker-machine 2>/dev/null; then
      eval $(docker-machine env default)
    fi
  fi
  if hash tewisay 2>/dev/null; then
    if hash fortune 2>/dev/null; then
      TEWSISAY_VARIATIONS=("te" "teh" "tes" "tewat")
      TEWI=${TEWSISAY_VARIATIONS[$(( $RANDOM % ${#TEWSISAY_VARIATIONS[@]} + 1 ))]}
      fortune | tewisay -f $TEWI 2>/dev/null
    fi
  elif hash cowsay 2>/dev/null; then
    if hash fortune 2>/dev/null; then
      fortune | cowsay -W 70 2>/dev/null
    fi
  fi
fi
