#!/bin/bash

# does not work for hot plugged keyboards (like for my netbook)
#xmodmap ~/.xmodmaprc
# nice, but conflicts with general Windows usage
#setxkbmap -option -option ctrl:nocaps,altwin:swap_alt_win
setxkbmap -option -option ctrl:nocaps

xset r rate 333 32

killall xbindkeys
xbindkeys
