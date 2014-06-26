#!/usr/bin/env python

import pyudev                                                                   
import subprocess
import time
import threading

ticks_before_reapplying_settings = 2

def kbd_event(action, device):
    global ticks_before_reapplying_settings

    if action == 'add':
        ticks_before_reapplying_settings = 2
        print(device)

def apply_keyboard_settings():
    global ticks_before_reapplying_settings

    while True:
        try:
            time.sleep(1)

            if ticks_before_reapplying_settings < 0:
                continue
            elif ticks_before_reapplying_settings > 0:
                ticks_before_reapplying_settings = ticks_before_reapplying_settings - 1
                continue

            ticks_before_reapplying_settings = -1

            subprocess.call('setxkbmap -option -option ctrl:nocaps -option altwin:swap_alt_win', shell = True)
            subprocess.call('xset r rate 333 32', shell = True)
            subprocess.call('killall xcape', shell = True)
            subprocess.call("xcape -t 333 -e 'Control_L=Escape;Shift_L=Shift_L|minus'", shell = True)
            subprocess.call('killall xbindkeys', shell = True)
            subprocess.call('xbindkeys', shell = True)

            print('applied keyboard settings')
        except:
            print('swallowed exception in apply_keyboard_settings thread')
            pass

def main():
    t = threading.Thread(target=apply_keyboard_settings)
    t.start()

    while True:
        try:
            context = pyudev.Context()
            monitor = pyudev.Monitor.from_netlink(context)
            monitor.filter_by('input')
            observer = pyudev.MonitorObserver(monitor, kbd_event)
            observer.start()

            for action, device in monitor:
                pass
        except:
            print('swallowed exception in main thread')
            pass

if __name__ == '__main__':
    main()
