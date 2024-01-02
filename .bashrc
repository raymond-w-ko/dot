export LANG=en_US.UTF-8
# enabling this can cause freeze when using ssh?
# export LC_ALL=en_US.UTF-8
# export LANGUAGE=en_US.UTF-8
# export LC_CTYPE=en_US.UTF-8
# export LC_NUMERIC=en_US.UTF-8
# export LC_TIME=en_US.UTF-8
# export LC_COLLATE=en_US.UTF-8
# export LC_MONETARY=en_US.UTF-8
# export LC_MESSAGES=en_US.UTF-8
# export LC_PAPER=en_US.UTF-8
# export LC_NAME=en_US.UTF-8
# export LC_ADDRESS=en_US.UTF-8
# export LC_TELEPHONE=en_US.UTF-8
# export LC_MEASUREMENT=en_US.UTF-8
# export LC_IDENTIFICATION=en_US.UTF-8

source /etc/profile

# prevent confusing tramp ssh
if [[ "$TERM" == "dumb" ]]; then
    PS1='$ '
    return
fi

# If not running interactively, don't do anything
[[ "$-" != *i* ]] && return

export HISTSIZE=32768
export HISTCONTROL=ignoreboth:erasedups

export GPG_TTY=$(tty)

appendpath () {
  case ":$PATH:" in
    *:"$1":*)
      ;;
    *)
      if [[ -d "$1" ]]; then
        PATH="${PATH:+$PATH:}$1"
      fi
  esac
}
prependpath () {
  case ":$PATH:" in
    *:"$1":*)
      ;;
    *)
      if [[ -d "$1" ]]; then
        # echo "$1"
        PATH="$1${PATH:+:$PATH}"
      fi
  esac
}
# higher priority path last, system default at the beginning
# echo $PATH
prependpath "/bin"
prependpath "/sbin"
prependpath "/usr/sbin"
prependpath "/usr/bin"
prependpath "/usr/local/sbin"
prependpath "/usr/local/bin"
prependpath "/usr/bin/core_perl"
if [[ -d  "/usr/lib/ccache/bin" ]]; then
  prependpath "/usr/lib/ccache/bin"
else
  prependpath "/usr/lib/ccache"
fi
prependpath "/usr/games"
prependpath "/usr/local/go/bin"

prependpath "/opt/mono/bin"
prependpath "/opt/aws/bin"
prependpath "/opt/tastyworks"

prependpath "$HOME/.config/yarn/global/node_modules/.bin"
prependpath "$HOME/npm-global/bin"
prependpath "$HOME/.nimble/bin"

prependpath "$HOME/src/neil"
prependpath "$HOME/.local/bin"
prependpath "$HOME/vim/bin"
prependpath "$HOME/nvim/bin"
prependpath "$HOME/emacs/bin"
prependpath "$HOME/go/bin"
unset appendpath
unset prependpath
# added by bootstrap script
# prependpath "$HOME/dot/bin"
# prependpath "$HOME/bin"
. ~/.bash.d/add_dot_extra_paths.sh

[[ -z "$TMP" ]] && export TMP='/tmp'
[[ -z "$TEMP" ]] && export TEMP='/tmp'

unameString=`uname -s`

# in rare cases of using Cygin, this is imported from the native Windows environment
# unset PYTHONHOME
# https://stackoverflow.com/questions/64570510/why-does-pip3-want-to-create-a-kdewallet-after-installing-updating-packages-on-u
export PYTHON_KEYRING_BACKEND=keyring.backends.null.Keyring

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

if hash stty 2>/dev/null; then
  stty sane
  stty stop undef
  stty start undef
  # breaks backspace in hidden password reading prompts
  # actually only if xterm backspace is set to generate ^H and not ^?
  # stty erase 
fi

# if [[ -n "$DISPLAY" ]]; then
  # export GTK_IM_MODULE=ibus
  # export XMODIFIERS=@im=ibus
  # export QT_IM_MODULE=ibus
# fi

if [[ -f ~/.bashrc.local ]]; then
  source ~/.bashrc.local
fi

function rko_startup_fortune {
  if hash tewisay 2>/dev/null; then
    if hash fortune 2>/dev/null; then
      TEWSISAY_VARIATIONS=("te" "teh" "tes" "tewat")
      TEWI=${TEWSISAY_VARIATIONS[$(( $RANDOM % ${#TEWSISAY_VARIATIONS[@]} + 1 ))]}
      fortune | perl -e'while(<>){$_ =~ s/\t/   /g; print;}' | tewisay -f $TEWI 2>/dev/null
    fi
  elif hash cowsay 2>/dev/null; then
    if hash fortune 2>/dev/null; then
      fortune | cowsay -W 70 2>/dev/null
    fi
  fi
}

if [[ ! -d /c/Windows && ! -d /mnt/c/Windows && -z $DISPLAY && $(tty) == /dev/tty1 ]]; then
  # exec sway-nvidia

  # we can't completely redirect stderr to a file otherwise root-less X breaks
  # I'm guessing it is determining the VTTY from the stderr file descriptor
  #startx | tee > "$HOME/.xsession-errors"

  # maybe this works from the archlinux wiki?
  exec startx -- -keeptty -nolisten tcp > "$HOME/.xsession-errors" 2>&1
elif [[ -f ~/src/interkonnect/interkonnect.py && $(ps auxww | grep interkonnect.py | grep -v grep | wc -l) = 0 ]]; then
  sudo ~/src/interkonnect/interkonnect.py
else
  if [[ "$unameString" == 'Darwin' ]]; then
    if hash docker-machine 2>/dev/null; then
      eval $(docker-machine env default)
    fi
  fi
  rko_startup_fortune
  # unfortunately --no-header option is too new
  #if [[ $(ps --no-header --pid=$PPID --format=comm) != "fish" && -z ${BASH_EXECUTION_STRING} ]]
  if [[ -f /usr/bin/fish && "$-" == *i* ]]; then
    exec /usr/bin/fish
  fi
  source $HOME/.bash.d/aliases.sh
fi

if [[ "$SHELL" == bash ]]; then
    source ~/.bash.d/ps1.sh
    # only for bash, not zsh
		export PROMPT_DIRTRIM=3
    # attempts to correct bad "cd" target
    shopt -s cdspell
    shopt -s checkwinsize
    shopt -s cmdhist
    shopt -s expand_aliases
    shopt -s extglob
    shopt -s histappend
    shopt -s autocd 2>/dev/null
fi
