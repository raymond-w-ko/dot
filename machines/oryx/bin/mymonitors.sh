#!/bin/bash

isMainExternalDpConnected=$(xrandr | grep 'HDMI-0 connected' | wc -l)
isLidOpen=$(cat /proc/acpi/button/lid/LID*/state | grep 'open' | wc -l)

echo isMainExternalDpConnected: $isMainExternalDpConnected
echo isLidOpen: $isLidOpen

INTERNAL_CONNECTOR="DP-0"
NO_PANNING="--panning 0x0"


if [ "$isMainExternalDpConnected" -eq 1 ] && [ "$isLidOpen" -eq 0 ]; then
  xrandr \
    --output HDMI-0 --primary --auto \
    --output DP-2 --left-of HDMI-0 --rotate left --auto \
    --output DP-4 --right-of HDMI-0 --auto \
    --output $INTERNAL_CONNECTOR --off
else
  echo "No monitors connected"
  xrandr \
    --output $INTERNAL_CONNECTOR --auto --primary $NO_PANNING \
    --output HDMI-0 --off \
    --output DP-1 --off \
    --output DP-2 --off \
    --output DP-3 --off \
    --output DP-4 --off
  killall compton
fi

#$configure_i3_workspaces.sh

# dropped my laptop and partially broke cooling, always want adaptive speed now.
#(sleep 20 && DISPLAY=":0.0" nvidia-settings -a [gpu:0]/GPUPowerMizerMode=1) &

nvidia-settings --load-config-only

# load color profile and DPMS settings
if [ "$isMainExternalDpConnected" -eq 1 ] && [ "$isLidOpen" -eq 0 ]; then
  # dispwin -d 1 "/home/rko/.local/share/icc/.icc"
  echo "Turning off screen blank"
  xset s off -dpms
else
  xset s 3600 3600 +dpms
fi

if [[ -d ~/wallpapers ]]; then
  feh --recursive --randomize --bg-fill ~/wallpapers
fi

picom --experimental-backends -b
