#!/bin/sh

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

$HOME/.xinitrc.extra

for d in /usr/share/fonts/*; do
  xset +fp "$d"
done
unset d
xset fp rehash

[[ -f ~/.Xresources ]] && xrdb ~/.Xresources
wmname LG3D
#xcalib
#xsetroot -cursor_name left_ptr &
xmodmap ~/.xmodmaprc &
# this causes a signficant amount of wakeups in powertop
#syndaemon -t -k -i 1 -d &
#xscreensaver -no-splash &
xbindkeys

xset r rate 333 32

hsetroot -full ~/Wallpapers/wallpaper.png &
#urxvtd -q -f -o
xcape -t 333 -e 'Control_L=Escape;Shift_L=Shift_L|minus'

# WM must start this for this to work?
#dropboxd

exec i3
