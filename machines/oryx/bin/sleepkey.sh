#!/bin/bash

sync
sync
sync

# pre-sleep stuff, otherwise network hangs
umountsbs

i3lock -c 000000
systemctl suspend
