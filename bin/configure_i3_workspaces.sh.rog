#!/bin/bash

isHdmiConnected=$(xrandr | grep 'HDMI-0 connected' | wc -l)
if [ "$isHdmiConnected" -eq 1 ]; then
  primaryWorkspace="HDMI-0"
else
  primaryWorkspace="LVDS-0"
fi

secondaryWorkspace="LVDS-0"

i3-msg workspace 1
i3-msg move workspace to output "$primaryWorkspace"

i3-msg workspace 2
i3-msg move workspace to output "$primaryWorkspace"

i3-msg workspace 3
i3-msg move workspace to output "$primaryWorkspace"

i3-msg workspace 4
i3-msg move workspace to output "$primaryWorkspace"

i3-msg workspace 5
i3-msg move workspace to output "$secondaryWorkspace"

i3-msg workspace 1
