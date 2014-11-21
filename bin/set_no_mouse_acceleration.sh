#!/usr/bin/env bash

set_no_mouse_accel()
{
  ids=$(xinput --list | awk -v search="$1" \
    '$0 ~ search {match($0, /id=[0-9]+/);\
                  if (RSTART) \
                    print substr($0, RSTART+3, RLENGTH-3)\
                 }'\
     )

  for i in $ids
  do
    echo $i
    xinput set-int-prop $i 'Device Accel Profile' 32 -1
    xinput set-atom-prop $i 'AccelerationScheme' 'none'
  done
}

set_no_mouse_accel "Logitech G500"
