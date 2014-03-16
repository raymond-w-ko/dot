#!/bin/bash
#x0vncserver -display :0 -passwordfile ~/.vnc/passwd
x11vnc -display :0 -rfbauth $HOME/.vnc/passwd -repeat
