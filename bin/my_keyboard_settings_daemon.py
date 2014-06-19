#!/usr/bin/env python

import pyudev                                                                   
from subprocess import call
import time

need_to_reapply_settings = 2

def kbd_event(action, device):
    global need_to_reapply_settings

    if action == 'add':
        need_to_reapply_settings = 2

def apply_settings():
    global need_to_reapply_settings
    need_to_reapply_settings = -1

    call('setxkbmap -option -option ctrl:nocaps', shell = True)
    call('xset r rate 333 32', shell = True)
    call('killall xcape', shell = True)
    call("xcape -t 333 -e 'Control_L=Escape;Shift_L=Shift_L|minus'", shell = True)
    call('killall xbindkeys', shell = True)
    call('xbindkeys', shell = True)

    print('applied keyboard settings')

monitor = pyudev.Monitor.from_netlink(pyudev.Context())
monitor.filter_by('input')
observer = pyudev.MonitorObserver(monitor, kbd_event)
observer.start()

while True:
    time.sleep(0.25)

    if need_to_reapply_settings < 0:
        continue

    need_to_reapply_settings = need_to_reapply_settings - 1
    if need_to_reapply_settings == 0:
        apply_settings()
