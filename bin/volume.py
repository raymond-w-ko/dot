#!/usr/bin/env python3

import sys
import re
import subprocess

# figure out if pulse audio is running
PULSEAUDIO_RUNNING = False
s = subprocess.Popen(["ps", "axw"], stdout=subprocess.PIPE)
for line in s.stdout:
    line = line.decode(encoding = 'UTF-8')
    if re.search('/usr/bin/pulseaudio', line):
        PULSEAUDIO_RUNNING = True

def adjust_volume(delta):
    if PULSEAUDIO_RUNNING:  
        verb = 'increase'
        if delta < 0:
            verb = 'decrease'
            delta = -delta
        new_volume = subprocess.Popen(["ponymix", verb, str(delta)], stdout=subprocess.PIPE).stdout
        new_volume = new_volume.read().decode(encoding = 'UTF-8')
        return new_volume.strip()
    else:
        pass

def show_volume(volume_string):
    if PULSEAUDIO_RUNNING:
        subprocess.Popen(['killall', 'xosd'], stderr=subprocess.PIPE).wait()
        subprocess.Popen(['killall', 'osd_cat'], stderr=subprocess.PIPE).wait()

        s = subprocess.Popen(['ponymix', 'is-muted'])
        s.wait()
        muted = ''
        if s.returncode != 1:
            muted = ' (Muted)'

        args = ['myxosd',
                '--barmode=percentage',
                '--percentage=%s' % volume_string,
                '--text=%s' % 'Volume: ' + volume_string + muted]
        subprocess.Popen(args)
    else:
        pass

def toggle_mute():
    if PULSEAUDIO_RUNNING:
        new_volume = subprocess.Popen(['ponymix', 'toggle'], stdout=subprocess.PIPE).stdout
        new_volume = new_volume.read().decode(encoding = 'UTF-8')
        return new_volume.strip()
    else:
        pass

delta = 5

if len(sys.argv) <= 1:
    sys.exit(0)
action = sys.argv[1]

if action == 'incr':
    new_volume = adjust_volume(delta)
    show_volume(new_volume)
elif action == 'decr':
    new_volume = adjust_volume(-delta)
    show_volume(new_volume)
elif action == 'toggle':
    new_volume = toggle_mute()
    show_volume(new_volume)
