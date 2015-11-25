#!/usr/bin/env python3
import os
import subprocess
import codecs
import re

def squarify(path):
  output = subprocess.check_output(["mediainfo", path]).decode("utf-8")
  tokens = re.split(r" |:|\n|\r", output)
  tokens = list(filter(lambda x: len(x) > 0, tokens))
  w = None
  h = None
  for i in range(len(tokens)):
    token = tokens[i]
    if token == "Width":
      w = int(tokens[i+1])
    elif token == "Height":
      h = int(tokens[i+1])
  
  maxdim = max(w, h)
  if (w != maxdim or h != maxdim):
    cmd = "mogrify -resize %dx%d! %s" % (maxdim, maxdim, path)
    cmd = ["mogrify", "-resize", "%dx%d!" % (maxdim, maxdim), path]
    print(" ".join(cmd))
    subprocess.check_output(cmd)
  

def main():
  for root, dirs, files in os.walk("."):
    for file in files:
      path = root + os.sep + file
      if file.endswith(".png"):
        squarify(path)

if __name__ == "__main__":
  main()
