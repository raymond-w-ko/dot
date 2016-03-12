#!/bin/bash

[ -f /etc/profile ] && source /etc/profile
[ -f /etc/xprofile ] && source /etc/xprofile

. "$HOME/.bash.d/add_dot_extra_paths.sh"

source ~/.bashrc

# system level
if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi
# override system defaults
#. ~/dot/bin/infinality-settings.sh

# machine specific customization
$HOME/.xinitrc.extra

# fonts
for d in /usr/share/fonts/*; do
  xset +fp "$d"
done
unset d
xset +fp $HOME/src/creep2
xset fp rehash

[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

# LOL, let's pretend we are Java Looking Glass 3D WM
wmname LG3D
hsetroot -solid '#000000'

# sync clipboard
autocutsel -fork &
autocutsel -selection PRIMARY -fork &

# WM must start this for this to work?
#dropboxd

# dbus is already launched?
#if hash dbus-launch 2>/dev/null; then
  #exec dbus-launch --sh-syntax --exit-with-session i3 -V -d all > ~/.i3/i3log-$(date +'%F-%k-%M-%S') 2>&1
#else
  #exec i3 -V -d all > ~/.i3/i3log-$(date +'%F-%k-%M-%S') 2>&1
#fi

export DE=gnome

dex /etc/xdg/autostart/at-spi-dbus-bus.desktop
#dex /etc/xdg/autostart/caribou-autostart.desktop
dex /etc/xdg/autostart/gsettings-data-convert.desktop
dex /etc/xdg/autostart/lxpolkit.desktop
# Start GNOME Keyring
eval $(/usr/bin/gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh)
# You probably need to do this too:
export GPG_AGENT_INFO
export SSH_AUTH_SOCK

export QT_STYLE_OVERRIDE="gtk"

#pulseaudio --start
start-pulseaudio-x11

my_keyboard_settings_daemon.py &

(sleep 5 && /opt/dropbox/dropbox) &

exec i3
#exec awesome
