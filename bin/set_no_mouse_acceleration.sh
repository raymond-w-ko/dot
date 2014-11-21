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
    xinput set-prop $i 'Device Accel Profile' -1
    xinput set-prop $i 'Device Accel Velocity Scaling' 1
    # don't think this is a valid setting, maybe only in X.org conf file?
    #xinput set-atom-prop $i 'AccelerationScheme' 'none'
  done
}

set_no_mouse_accel "Logitech G500"
