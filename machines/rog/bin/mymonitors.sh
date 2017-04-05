#!/bin/bash
set -x

isHdmiConnected=$(xrandr | grep 'HDMI-0 connected' | wc -l)
isDpOneConnected=$(xrandr | grep 'DP-1 connected' | wc -l)
isLidOpen=$(cat /proc/acpi/button/lid/LID/state | grep 'open' | wc -l)

MODE="3840x2160_R"
xrandr --rmmode "$MODE"
xrandr --newmode "$MODE" 533.00  3840 3888 3920 4000  2160 2163 2168 2222 +hsync -vsync

if [ "$isDpOneConnected" -eq 1 ] && [ "$isLidOpen" -eq 1 ]; then
  echo "DisplayPort and Laptop Screen"
  xrandr --addmode DP-1 "$MODE"
  xrandr \
    --output LVDS-0 --auto \
    --output DP-1 --primary --right-of LVDS-0 --mode "$MODE"
elif [ "$isDpOneConnected" -eq 1 ] && [ "$isLidOpen" -eq 0 ]; then
  echo "DisplayPort Only"
  xrandr --addmode DP-1 "$MODE"
  xrandr \
    --output LVDS-0 --off \
    --output DP-1 --primary --mode "$MODE"
elif [ "$isHdmiConnected" -eq 1 ] && [ "$isLidOpen" -eq 1 ]; then
  echo "HDMI and Laptop Screen"
  xrandr --addmode HDMI-0 "$MODE"
  xrandr \
    --output LVDS-0 --auto \
    --output HDMI-0 --primary --right-of LVDS-0 --mode "$MODE"
elif [ "$isHdmiConnected" -eq 1 ] && [ "$isLidOpen" -eq 0 ]; then
  echo "HDMI only"
  xrandr --addmode HDMI-0 "$MODE"
  # xrandr \
  #   --output LVDS-0 --off \
  #   --output HDMI-0 --primary --mode "$MODE"
  xrandr \
    --output LVDS-0 --off \
    --output HDMI-0 --primary --auto
else
  echo "No monitors connected"
  xrandr \
    --output LVDS-0 --auto --primary \
    --output VGA-0 --off \
    --output HDMI-0 --off \
    --output DP-0 --off \
    --output DP-1 --off
fi

#$configure_i3_workspaces.sh

# dropped my laptop and partially broke cooling, always want adaptive speed now.
#(sleep 20 && DISPLAY=":0.0" nvidia-settings -a [gpu:0]/GPUPowerMizerMode=1) &

nvidia-settings --load-config-only

# load color profile
if [ "$isDpOneConnected" -eq 1 ] && [ "$isLidOpen" -eq 0 ]; then
  # dispwin -d 1 $HOME/.local/share/icc/SE42UMS\ \#1\ 2017-03-09\ 13-36\ 2.2\ F-S\ XYZLUT+MTX.icc
  xset s off -dpms
else
  xset s 3600 3600 +dpms
fi
