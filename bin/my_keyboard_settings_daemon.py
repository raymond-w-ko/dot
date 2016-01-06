#!/usr/bin/env python

import pyudev                                                                   
import subprocess
import time
import threading
import getpass

ticks_before_reapplying_settings = 2

username = getpass.getuser()

def system(cmd):
    subprocess.call(cmd, shell = True)

def apply_keyboard_settings():
    global ticks_before_reapplying_settings

    while True:
        try:
            time.sleep(0.25)

            if ticks_before_reapplying_settings < 0:
                continue
            elif ticks_before_reapplying_settings > 0:
                ticks_before_reapplying_settings = ticks_before_reapplying_settings - 1
                continue

            ticks_before_reapplying_settings = -1

            system('xmodmap ~/.Xmodmap')
            system('xset r rate 333 32')
            # not necessary with Kinesis Advantage keyboard
            # system('killall -u %s xcape' % username)
            # system("xcape -t 333 -e 'Control_R=Return'")
            system('killall -u %s xbindkeys' % username)
            system('xbindkeys')
            system('set_no_mouse_acceleration.sh')

            print('applied keyboard settings')
        except:
            print('swallowed exception in apply_keyboard_settings thread')

def main():
    global ticks_before_reapplying_settings

    t = threading.Thread(target=apply_keyboard_settings)
    t.start()

    while True:
        try:
            context = pyudev.Context()
            monitor = pyudev.Monitor.from_netlink(context)
            monitor.filter_by('input')

            for action, device in monitor:
                if action == 'add':
                    ticks_before_reapplying_settings = 2

        except KeyboardInterrupt:
            break
        except:
            print('swallowed exception in main thread')
            pass

if __name__ == '__main__':
    main()
