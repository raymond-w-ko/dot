#!/bin/bash

if hash kitty 2>/dev/null ; then
  exec kitty
elif hash st 2>/dev/null ; then
  exec st
elif hash urxvt 2>/dev/null; then
  exec urxvt
else
  exec xterm
fi
