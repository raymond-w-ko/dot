#!/bin/sh

chmod -e

ffmpeg -i "$1" -ar 44100 tmp.wav 
mv tmp.wav "$1"
