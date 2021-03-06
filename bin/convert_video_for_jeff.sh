#!/bin/bash

set -x

#REM mp4  (H.264 / ACC)
# -i %1 -b 1500k -vcodec libx264 -vpre slow -vpre baseline -g 30 -s 640x360 %1.mp4
#REM webm (VP8 / Vorbis)
# -i %1 -b 1500k -vcodec libvpx -acodec libvorbis -ab 160000 -f webm -g 30 -s 640x360 %1.webm
#REM ogv  (Theora / Vorbis)
# -i %1 -b 1500k -vcodec libtheora -acodec libvorbis -ab 160000 -g 30 -s 640x360 %1.ogv
#REM jpeg (screenshot at 10 seconds)
# -i %1 -ss 00:10 -vframes 1 -r 1 -s 640x360 -f image2 %1.jpg

create_dist_mp4()
{
  SRC="$1"
  DST=converted_for_publish/${SRC%.*}.mp4

  echo creating $DST

  rm -f "$DST"
  nice -n 19 ffmpeg \
    -i "$SRC" \
    -threads 8 \
    -strict -2 \
    -vcodec libx264 -b:v 1000000k -crf 16 \
    -profile:v baseline -level:v 3.0 -pix_fmt yuv420p \
    -movflags +faststart \
    -acodec aac \
    -b:a 128k \
    -ar 44100 \
    "$DST" \
    </dev/null
}

create_master_mp4()
{
  SRC="$1"
  DST=converted_for_editing/${SRC%.*}.mp4

  echo creating $DST

  rm -f "$DST"
  nice -n 19 ffmpeg \
    -i "$SRC" \
    -threads 8 \
    -strict -2 \
    -vcodec libx264 -b:v 1000000k -crf 1 \
    -profile:v baseline -level:v 3.0 -pix_fmt yuv420p \
    -movflags +faststart \
    -acodec aac \
    -b:a 128k \
    -ar 44100 \
    "$DST" \
    </dev/null
}

if [[ "$1" == "editing" ]]; then
  mkdir -p converted_for_editing
  find . -iname '*.mp4' | while read file; do
    create_master_mp4 "$file"
  done
elif [[ "$1" == "publishing" ]]; then
  mkdir -p converted_for_publish
  find . -iname '*.wmv' | while read file; do
    create_dist_mp4 "$file"
  done
else
  echo "$0" "[editing | publishing]"
fi

