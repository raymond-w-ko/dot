#!/usr/bin/env python3

import sys
import subprocess
import getpass
import threading
from threading import Timer

import pyudev

username = getpass.getuser()


def debounce(wait):
    """ Decorator that will postpone a functions
        execution until after wait seconds
        have elapsed since the last time it was invoked. """
    def decorator(fn):
        def debounced(*args, **kwargs):
            def call_it():
                fn(*args, **kwargs)
            try:
                debounced.t.cancel()
            except(AttributeError):
                pass
            debounced.t = Timer(wait, call_it)
            debounced.t.start()
        return debounced
    return decorator


def system(cmd):
    subprocess.call(cmd, shell=True)


@debounce(2)
def apply_keyboard_settings():
    print("%s re-applying keyboard and mouse settings" % (sys.argv[0]))
    system('xset r rate 333 32')
    system('killall -u %s xbindkeys' % (username))
    system('xbindkeys')


def main():
    apply_keyboard_settings()

    context = pyudev.Context()
    monitor = pyudev.Monitor.from_netlink(context)
    monitor.filter_by('input')
    for action, device in monitor:
        if action == 'add':
            t = threading.Timer(1.0, apply_keyboard_settings)
            t.start()


if __name__ == '__main__':
    main()
