#!/bin/bash

for d in /usr/share/fonts/*; do
  pushd "$d"
  sudo mkfontdir
  popd
  xset +fp "$d"
done
