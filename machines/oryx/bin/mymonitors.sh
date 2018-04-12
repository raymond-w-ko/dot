#!/bin/bash

isHdmiConnected=$(xrandr | grep 'HDMI-0 connected' | wc -l)
isMainExternalDpConnected=$(xrandr | grep 'DP-1 connected' | wc -l)
isLidOpen=$(cat /proc/acpi/button/lid/LID*/state | grep 'open' | wc -l)

echo isHdmiConnected: $isHdmiConnected
echo isMainExternalDpConnected: $isMainExternalDpConnected
echo isLidOpen: $isLidOpen

INTERNAL_CONNECTOR="DP-0"
NO_PANNING="--panning 0x0"

# proper
# MODE="3840x2160_60.00"
# xrandr --rmmode "$MODE"
# xrandr --newmode "$MODE" 712.75  3840 4160 4576 5312  2160 2163 2168 2237 -hsync +vsync
# reduced blanking
MODE="3840x2160_R"
xrandr --rmmode "$MODE"
xrandr --newmode "$MODE" 533.00  3840 3888 3920 4000  2160 2163 2168 2222 +hsync -vsync

if [ "$isMainExternalDpConnected" -eq 1 ] && [ "$isLidOpen" -eq 1 ]; then
  echo "DisplayPort and Laptop Screen"
  xrandr --addmode DP-1 "$MODE"
  xrandr \
    --output $INTERNAL_CONNECTOR --auto $NO_PANNING \
    --output DP-1 --primary --right-of $INTERNAL_CONNECTOR --mode "$MODE" $NO_PANNING
elif [ "$isMainExternalDpConnected" -eq 1 ] && [ "$isLidOpen" -eq 0 ]; then
  echo "DisplayPort Only"
  xrandr --addmode DP-1 "$MODE"
  xrandr \
    --output $INTERNAL_CONNECTOR --off \
    --output DP-1 --primary --mode "$MODE" $NO_PANNING
elif [ "$isHdmiConnected" -eq 1 ] && [ "$isLidOpen" -eq 1 ]; then
  echo "HDMI and Laptop Screen"
  xrandr --addmode HDMI-0 "$MODE"
  xrandr \
    --output $INTERNAL_CONNECTOR --auto $NO_PANNING \
    --output HDMI-0 --primary --right-of $INTERNAL_CONNECTOR --mode "$MODE" $NO_PANNING
elif [ "$isHdmiConnected" -eq 1 ] && [ "$isLidOpen" -eq 0 ]; then
  echo "HDMI only"
  xrandr --addmode HDMI-0 "$MODE"
  xrandr \
    --output $INTERNAL_CONNECTOR --off \
    --output HDMI-0 --primary --mode "$MODE" $NO_PANNING
else
  echo "No monitors connected"
  xrandr --rmmode "$MODE"
  xrandr \
    --output $INTERNAL_CONNECTOR --auto --primary $NO_PANNING \
    --output HDMI-0 --off \
    --output DP-1 --off \
    --output DP-2 --off \
    --output DP-3 --off \
    --output DP-4 --off
fi

#$configure_i3_workspaces.sh

# dropped my laptop and partially broke cooling, always want adaptive speed now.
#(sleep 20 && DISPLAY=":0.0" nvidia-settings -a [gpu:0]/GPUPowerMizerMode=1) &

nvidia-settings --load-config-only

# load color profile and DPMS settings
if [ "$isMainExternalDpConnected" -eq 1 ] && [ "$isLidOpen" -eq 0 ]; then
  # dispwin -d 1 "/home/rko/.local/share/icc/SE42UMS #1 2018-03-27 13-00 D6500 2.2 F-S XYZLUT+MTX.icc"
  echo "Turning off screen blank"
  xset s off -dpms
elif [ "$isHdmiConnected" -eq 1 ] && [ "$isLidOpen" -eq 0 ]; then
  echo "Turning off screen blank"
  xset s off -dpms
else
  xset s 3600 3600 +dpms
fi

if [[ -d ~/wallpapers ]]; then
  wallpaper=$(find ~/wallpapers -type f | shuf -n 1)
  feh --bg-center "$wallpaper"
fi
