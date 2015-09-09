#!/bin/bash

case "$(uname -s)" in
   Darwin)
     ;;

   Linux)
     nc 127.0.0.1 64999 <&0
     ;;

   CYGWIN*)
     ;;
     
   MINGW32*|MSYS*)
     ;;

   *)
     ;;
esac
