#!/bin/sh

MEMORY_PERCENT=$(memory_pressure | awk '/System-wide memory free percentage/ {print 100 - $5}' | sed 's/%//')

sketchybar --set $NAME label="${MEMORY_PERCENT}%"
