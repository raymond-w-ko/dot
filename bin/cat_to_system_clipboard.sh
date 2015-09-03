#!/bin/bash

case "$(uname -s)" in
   Darwin)
     ;;

   Linux)
     ;;

   CYGWIN*)
     ;;
     
   MINGW32*|MSYS*)
     ;;

   *)
     ;;
esac
