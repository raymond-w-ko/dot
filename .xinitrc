#!/bin/bash

[ -f /etc/xprofile ] && source /etc/xprofile
[ -f ~/.xprofile ] && source ~/.xprofile
exec i3 -V > ~/.i3/i3log
