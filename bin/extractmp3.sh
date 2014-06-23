#!/bin/bash

outext=mp3
for file in "$@"; do
  ext=${file##*.}
  fname=`basename "$file" "$ext"`
  ffmpeg -i "$file" -b:a 192K -vn "$fname$outext"
done
