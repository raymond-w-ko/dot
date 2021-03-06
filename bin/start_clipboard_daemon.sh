#!/bin/bash -e

_term()
{
  echo "caught signal, killing clipboard daemons"
  kill -TERM "$child1" 2>/dev/null
}

trap _term SIGTERM
trap _term SIGINT

clear
echo "starting network -> clipboard subshell loop"
case "$(uname -s)" in
   Darwin)
     echo "ERROR: do no know how to start clipboard daemon"
     exit 1
     ;;

   Linux)
     echo "ERROR: do no know how to start clipboard daemon"
     exit 1
     ;;

   CYGWIN*)
     (
     while true; do
       nc -l 127.0.0.1 64999 | putclip -d
       echo "somebody wrote to the clipboard"
     done
     )
     ;;
     
   MINGW32*|MSYS*)
     echo "ERROR: do no know how to start clipboard daemon"
     exit 1
     ;;

   *)
     echo "ERROR: do no know how to start clipboard daemon"
     exit 1
     ;;
esac

child1=$!
wait "$child1"
