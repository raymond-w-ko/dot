#!/bin/sh

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
wmname LG3D
#xsetroot -cursor_name left_ptr &
xmodmap ~/.xmodmaprc &
xbindkeys

xset r rate 333 32

hsetroot -full ~/Wallpapers/wallpaper.png &
xcape -t 333 -e 'Control_L=Escape;Shift_L=Shift_L|minus'

# WM must start this for this to work?
#dropboxd

exec i3
