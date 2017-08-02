#!/bin/sh

# Flashes the active window.
# Requires transset-df and a composite manager, like xcompmgr.

transset-df -a -m 0.0 -x 0.75 &> /dev/null
sleep .1
transset-df -a -m 1.0 -x 1.0 &> /dev/null
