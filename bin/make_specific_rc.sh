#!/bin/bash

if [ -z "$1" ]; then
  echo $0 [rc suffx]
fi

SUFFIX="$1"

FILE=~/.gtkrc-2.0.$SUFFIX
if [ ! -e "$FILE" ]; then
  echo 'gtk-theme-name = "MediterraneanNightDarkest"' >> "$FILE"
  echo 'gtk-font-name = "DejaVu Sans 8"' >> "$FILE"
fi

FILE=~/.i3status.conf.$SUFFIX
if [ ! -e "$FILE" ]; then
  cp ~/.i3status.conf.vm0 >> "$FILE"
fi

FILE=~/.xinitrc.$SUFFIX
if [ ! -e "$FILE" ]; then
  echo '#!/bin/bash' >> "$FILE"
  chmod +x "$FILE"
fi

FILE=~/bin/mymonitors.sh.$SUFFIX
if [ ! -e "$FILE" ]; then
  echo '#!/bin/bash' > "$FILE"
  chmod +x "$FILE"
fi

FILE=~/bin/configure_i3_workspaces.sh.$SUFFIX
if [ ! -e "$FILE" ]; then
  echo '#!/bin/bash' > "$FILE"
  chmod +x "$FILE"
fi
