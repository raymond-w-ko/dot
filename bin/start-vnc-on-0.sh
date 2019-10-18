#!/bin/bash
x11vnc -forever -noxdamage -display :0 -rfbauth $HOME/.vnc/passwd -repeat
