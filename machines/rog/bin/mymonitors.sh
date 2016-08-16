#!/bin/bash
set -x

isHdmiConnected=$(xrandr | grep 'HDMI-0 connected' | wc -l)
isDpOneConnected=$(xrandr | grep 'DP-1 connected' | wc -l)
isLidOpen=$(cat /proc/acpi/button/lid/LID/state | grep 'open' | wc -l)

MODE=3840x2160_30.00
xrandr --rmmode "$MODE"
xrandr --newmode "$MODE" 339.57  3840 4080 4496 5152  2160 2161 2164 2197  -HSync +Vsync

if [ "$isDpOneConnected" -eq 1 ] && [ "$isLidOpen" -eq 1 ]; then
  echo "DisplayPort and Laptop Screen"
  xrandr --addmode DP-1 "$MODE"
  xrandr \
    --output LVDS-0 --auto \
    --output DP-1 --primary --right-of LVDS-0 --auto
elif [ "$isDpOneConnected" -eq 1 ] && [ "$isLidOpen" -eq 0 ]; then
  echo "DisplayPort Only"
  xrandr --addmode DP-1 "$MODE"
  xrandr \
    --output LVDS-0 --off \
    --output DP-1 --primary --auto
elif [ "$isHdmiConnected" -eq 1 ] && [ "$isLidOpen" -eq 1 ]; then
  echo "HDMI and Laptop Screen"
  xrandr --addmode HDMI-0 "$MODE"
  xrandr \
    --output LVDS-0 --auto \
    --output HDMI-0 --primary --right-of LVDS-0 --auto
elif [ "$isHdmiConnected" -eq 1 ] && [ "$isLidOpen" -eq 0 ]; then
  echo "HDMI only"
  xrandr \
    --output LVDS-0 --off \
    --output HDMI-0 --auto --primary
else
  echo "No monitors connected"
  xrandr \
    --output LVDS-0 --auto --primary \
    --output VGA-0 --off \
    --output HDMI-0 --off \
    --output DP-1 --off
fi

#$configure_i3_workspaces.sh
# dropped my laptop and partially broke cooling, always want adaptive speed now.
nvidia-settings --load-config-only
#(sleep 20 && DISPLAY=":0.0" nvidia-settings -a [gpu:0]/GPUPowerMizerMode=1) &
