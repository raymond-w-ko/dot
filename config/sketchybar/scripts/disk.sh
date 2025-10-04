#!/bin/sh

DISK_PERCENT=$(df -h / | awk 'NR==2 {print $5}' | sed 's/%//')

sketchybar --set $NAME label="${DISK_PERCENT}%"
