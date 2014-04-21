unameString=`uname -s`

MD5="md5sum"
if [[ "$unameString" == 'Darwin' ]]; then
  MD5="md5"
fi
HOSTNAME=`hostname -s`
HASH=`echo $HOSTNAME | ${MD5}`
RGB=${HASH:0:6}
COLORED_HOST=$(~/bin/rgb2term.py $RGB)$HOSTNAME

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
  ROOT_WARNING="$(if [[ ${EUID} == 0 ]]; then echo "$redBold*** ROOT *** "; else echo ''; fi)"
  USER_AT_HOST="$(if [[ ${EUID} == 0 ]]; then echo "$redBold\u"; else echo "$blue\u"; fi)$blue @ $COLORED_HOST"
  BATTERY="\$(BatteryStatus)"
  FILE_INFO="\$(ls -1 2>/dev/null | wc -l | sed 's: ::g') files, \$(ls -lah 2>/dev/null | grep -m 1 total | sed 's/total //')b"
  LINE1="$ROOT_WARNING$whiteBold($cyan\D{%Y %b %e %l:%M:%S %p}$whiteBold)$dash($green$BATTERY$whiteBold)"
  #LINE2="($yellow$FILE_INFO$whiteBold)$dash$green\$(GitBranch)$whiteBold$dash($yellow\w$white$whiteBold)"
  LINE2="$ROOT_WARNING$green\$(GitBranch)$whiteBold$dash$green\$(HgBranch)$whiteBold$dash($yellow\w$white$whiteBold)"
  LINE3="$ROOT_WARNING$whiteBold$USER_AT_HOST$whiteBold ($white$RET_STATUS$whiteBold)"
  PROMPT=" $dash> $normalColor"
  export PS1="\n$LINE1\n$LINE2\n$LINE3$PROMPT`echo $REAL_LAST_RET`"
}
export PROMPT_COMMAND=prompt_command
