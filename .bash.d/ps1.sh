unameString=`uname -s`

MD5="md5sum"
if [[ "$unameString" == 'Darwin' ]]; then
  MD5="md5"
fi
HOSTNAME=`hostname -s`
HASH=`echo $HOSTNAME | ${MD5}`
RGB=${HASH:0:6}
COLORED_HOST=$(rgb2term.py $RGB)$HOSTNAME

_WhiteBold="\[\033[1;37m\]"
_White="\[\033[0;37m\]"
_BlueBold="\[\033[1;34m\]"
_Blue="\[\033[0;34m\]"
_GreenBold="\[\033[1;32m\]"
_Green="\[\033[0;32m\]"
_RedBold="\[\033[1;31m\]"
_YellowBold="\[\033[1;33m\]"
_Yellow="\[\033[0;33m\]"
_CyanBold="\[\033[1;36m\]"
_Cyan="\[\033[0;36m\]"
_Purple="\[\033[0;35m\]"
_PurpleBold="\[\033[1;35m\]"
_NormalColor="\[\033[0m\]"
#_Dash="\342\224\200"
_Dash="-"

function prompt_command {
  ACTUAL_LAST_RET=$?

  RET_STATUS="\$(if [[ $ACTUAL_LAST_RET == 0 ]]; then echo -n \"$_GreenBold\"; echo ret: $ACTUAL_LAST_RET; else echo -n \"$_RedBold\"; echo ret: $ACTUAL_LAST_RET; fi)"
  function BatteryStatus {
    if hash acpi 2>/dev/null; then
      acpi | sed 's/Battery 0: //' | sed 's/ remaining//'
    else
      echo "no acpi"
    fi
  }
  function GitBranch {
    if hash git 2>/dev/null; then
      GIT_BRANCH=`git branch 2>/dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/'`
      if [[ ! -z "$GIT_BRANCH" ]]; then
        echo "(git: $GIT_BRANCH)"
      else
        echo ""
      fi
    else
      echo ""
    fi
  }
  function HgBranch {
    if hash hg 2>/dev/null; then
      HG_BRANCH=`hg branch 2>/dev/null`
      if [[ ! -z "$HG_BRANCH" ]]; then
        echo "(hg: $HG_BRANCH)"
      else
        echo ""
      fi
    else
      echo ""
    fi
  }
  ROOT_WARNING="$(if [[ ${EUID} == 0 ]]; then echo "$_RedBold*** ROOT *** "; else echo ''; fi)"
  USER_AT_HOST="$(if [[ ${EUID} == 0 ]]; then echo "$_RedBold\u"; else echo "$_Blue\u"; fi)$_Blue @ $COLORED_HOST"
  BATTERY="\$(BatteryStatus)"
  FILE_INFO="\$(ls -1 2>/dev/null | wc -l | sed 's: ::g') files, \$(ls -lah 2>/dev/null | grep -m 1 total | sed 's/total //')b"
  LINE1="$ROOT_WARNING$_WhiteBold($_Cyan\D{%Y %b %e %l:%M:%S %p}$_WhiteBold)$_Dash($_Green$BATTERY$_WhiteBold)"
  #LINE2="($_Yellow$FILE_INFO$_WhiteBold)$_Dash$_Green\$(GitBranch)$_WhiteBold$_Dash($_Yellow\w$_White$_WhiteBold)"
  LINE2="$ROOT_WARNING$_Green\$(GitBranch)$_WhiteBold$_Dash$_Green\$(HgBranch)$_WhiteBold$_Dash($_Yellow\w$_White$_WhiteBold)"
  LINE3="$ROOT_WARNING$_WhiteBold$USER_AT_HOST$_WhiteBold ($_White$RET_STATUS$_WhiteBold)"
  PROMPT=" $_Dash> $_NormalColor"
  export PS1="\n$LINE1\n$LINE2\n$LINE3$PROMPT`echo $REAL_LAST_RET`"
}
export PROMPT_COMMAND=prompt_command
