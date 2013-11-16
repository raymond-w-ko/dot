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

[[ -f ~/.Xresources ]] && xrdb ~/.Xresources
xrandr --dpi 156
xset +fp /usr/share/fonts/artwiz-fonts
xset fp rehash
# ???
wmname LG3D
# TODO
#xcalib
xsetroot -cursor_name left_ptr &
xmodmap ~/.xmodmaprc &
syndaemon -t -k -i 1 -d &
xscreensaver -no-splash &
xbindkeys

xinput set-int-prop "ETPS/2 Elantech Touchpad" "Device Accel Profile" 32 -1
xinput set-float-prop "ETPS/2 Elantech Touchpad" "Device Accel Constant Deceleration" 4.0
xinput set-float-prop "ETPS/2 Elantech Touchpad" "Device Accel Adaptive Deceleration" 2.0
xset m 1 0
xset r rate 333 25

hsetroot -full ~/Wallpapers/wallpaper &

DEFAULT_SESSION=xmonad
case $DEFAULT_SESSION in
  xmonad)
    exec dbus-launch xmonad
    ;;
esac
