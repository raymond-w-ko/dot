#!/bin/bash
x11vnc -forever -display :0 -rfbauth $HOME/.vnc/passwd -repeat
