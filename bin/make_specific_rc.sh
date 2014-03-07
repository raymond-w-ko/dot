#!/bin/bash

if [ -z "$1" ]; then
  echo $0 [rc suffx]
fi

SUFFIX="$1"

echo 'gtk-theme-name = "MediterraneanNightDarkest"' > ~/.gtkrc-2.0.$SUFFIX
echo 'gtk-font-name = "DejaVu Sans 8"' >> ~/.gtkrc-2.0.$SUFFIX

cp ~/.i3status.conf.vm0 ~/.i3status.conf.$SUFFIX

echo '#!/bin/sh' > ~/.xinitrc.$SUFFIX
chmod +x ~/.xinitrc.$SUFFIX
