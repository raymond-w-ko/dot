#!/bin/bash
isHdmiConnected=$(xrandr | grep 'HDMI-0 connected' | wc -l)
isDpOneConnected=$(xrandr | grep 'DP-1 connected' | wc -l)
isLidOpen=$(cat /proc/acpi/button/lid/LID/state | grep 'open' | wc -l)

if [ "$isDpOneConnected" -eq 1 ] && [ "$isLidOpen" -eq 1 ]; then
  echo "DisplayPort and Laptop Screen"
  xrandr \
    --output LVDS-0 --auto \
    --output DP-1 --auto --primary --right-of LVDS-0
elif [ "$isDpOneConnected" -eq 1 ] && [ "$isLidOpen" -eq 0 ]; then
  echo "DisplayPort Only"
  xrandr \
    --output LVDS-0 --off \
    --output DP-1 --auto --primary
elif [ "$isHdmiConnected" -eq 1 ] && [ "$isLidOpen" -eq 1 ]; then
  echo "HDMI and Laptop Screen"
  xrandr \
    --output LVDS-0 --auto \
    --output HDMI-0 --auto --primary --right-of LVDS-0
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
