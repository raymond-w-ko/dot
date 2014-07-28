#!/bin/bash
#import -window "$(xdotool getwindowfocus -f)" $HOME/tmp/$(date +%F_%H%M%S_%N).png

activeWinLine=$(xprop -root | grep "_NET_ACTIVE_WINDOW(WINDOW)")
activeWinId=${activeWinLine:40}
import -window "$activeWinId" $HOME/tmp/$(date +%F_%H_%M_%S_%N).png
