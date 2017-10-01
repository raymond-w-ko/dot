#!/bin/bash

[ -f /etc/profile ] && source /etc/profile
[ -f /etc/xprofile ] && source /etc/xprofile
source "$HOME/.bash.d/add_dot_extra_paths.sh"
source ~/.bashrc

sysresources=/etc/X11/xinit/.Xresources
userresources=$HOME/.Xresources

# merge in defaults and keymaps
if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi
if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

# fonts
for d in /usr/share/fonts/*; do
  xset +fp "$d"
done
unset d
xset +fp $HOME/src/creep2
xset fp rehash

# system level
if [ -d /etc/X11/xinit/xinitrc.d ] ; then
  for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# machine specific customization
$HOME/.xinitrc.extra

# LOL, let's pretend we are Java Looking Glass 3D WM
wmname LG3D
hsetroot -solid '#000000'

# clipboard manager
systemctl --user start clipmenud

export DE=gnome

dex /etc/xdg/autostart/at-spi-dbus-bus.desktop
# dex /etc/xdg/autostart/caribou-autostart.desktop
dex /etc/xdg/autostart/gsettings-data-convert.desktop
dex /etc/xdg/autostart/lxpolkit.desktop
# Start GNOME Keyring
eval $(/usr/bin/gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh)
export GPG_AGENT_INFO
export SSH_AUTH_SOCK

export QT_STYLE_OVERRIDE="gtk"

#pulseaudio --start
start-pulseaudio-x11

ibus-daemon -drx &
my_keyboard_settings_daemon.py &

exec i3
