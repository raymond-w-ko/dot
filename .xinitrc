#!/bin/bash

# system level
if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# machine specific customization
$HOME/.xinitrc.extra

# fonts
for d in /usr/share/fonts/*; do
  xset +fp "$d"
done
unset d
xset fp rehash

[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

# LOL, let's pretend we are Java Looking Glass 3D WM
wmname LG3D

xbindkeys
~/bin/keyboard_settings.sh

hsetroot -solid '#000000'
xcape -t 333 -e 'Control_L=Escape;Shift_L=Shift_L|minus'

# WM must start this for this to work?
#dropboxd

# dbus is already launched?
#if hash dbus-launch 2>/dev/null; then
  #exec dbus-launch --sh-syntax --exit-with-session i3 -V -d all > ~/.i3/i3log-$(date +'%F-%k-%M-%S') 2>&1
#else
  #exec i3 -V -d all > ~/.i3/i3log-$(date +'%F-%k-%M-%S') 2>&1
#fi

/usr/lib/lxpolkit/lxpolkit &

exec i3 -V > ~/.i3/i3log-$(date +'%F-%k-%M-%S') 2>&1
