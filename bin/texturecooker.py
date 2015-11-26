#!/usr/bin/env python3
import os
import subprocess
import codecs
import re

pvrtextool = "/opt/Imagination/PowerVR_Graphics/PowerVR_Tools/PVRTexTool/CLI/Linux_x86_64/PVRTexToolCLI"
nvcompress = "/usr/bin/nvcompress"

def cook(path):
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
    
  # make DDS
  # out = re.sub(r".png$", ".dds", path)
  # cmd = [nvcompress, "-bc3", "-alpha", path, out]
  # print(" ".join(cmd))
  # subprocess.check_output(cmd)
  
  # make PVR
  out = re.sub(r".png$", ".pvr", path)
  cmd = [pvrtextool, "-i", path, "-o", out, "-f", "PVRTC1_4", "-q", "pvrtcbest"]
  print(" ".join(cmd))
  subprocess.check_output(cmd)

def main():
  for root, dirs, files in os.walk("."):
    for file in files:
      path = root + os.sep + file
      if file.endswith(".png"):
        cook(path)

if __name__ == "__main__":
  main()
