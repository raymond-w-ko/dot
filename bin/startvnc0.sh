#!/bin/bash
#x0vncserver -display :0 -passwordfile ~/.vnc/passwd
x11vnc -forever -display :0 -rfbauth $HOME/.vnc/passwd -repeat -ncache 10 -ncache_cr
