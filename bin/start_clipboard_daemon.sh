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
(
while true; do
  nc -l 127.0.0.1 64999 | putclip
done
)

child1=$!
wait "$child1"
