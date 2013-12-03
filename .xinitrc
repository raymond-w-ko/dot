#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

xrandr --dpi 156

xset +fp /usr/share/fonts/artwiz-fonts
xset +fp /usr/share/fonts/local
xset +fp /usr/share/fonts/misc
xset fp rehash

[[ -f ~/.Xresources ]] && xrdb ~/.Xresources
wmname LG3D
#xcalib
#xsetroot -cursor_name left_ptr &
xmodmap ~/.xmodmaprc &
# this causes a signficant amount of wakeups in powertop
#syndaemon -t -k -i 1 -d &
xscreensaver -no-splash &
xbindkeys

xinput set-int-prop "ETPS/2 Elantech Touchpad" "Device Accel Profile" 32 2 
xinput set-float-prop "ETPS/2 Elantech Touchpad" "Device Accel Constant Deceleration" 10.0
xinput set-float-prop "ETPS/2 Elantech Touchpad" "Device Accel Adaptive Deceleration" 2.0
xset r rate 333 25

hsetroot -full ~/Wallpapers/wallpaper &
urxvtd -q -f -o
xcape -t 333 -e 'Control_L=Escape;Shift_L=Shift_L|minus'

exec i3
