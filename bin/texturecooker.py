#!/usr/bin/env python3
import os
import subprocess
import codecs
import re
import hashlib
import shutil

pvrtextool = "/opt/Imagination/PowerVR_Graphics/PowerVR_Tools/PVRTexTool/CLI/Linux_x86_64/PVRTexToolCLI"
nvcompress = "/usr/bin/nvcompress"
cache_dir = os.environ["HOME"] + os.sep + ".cache" + os.sep + "texturecooker"

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
    
  h = None
  safe_path = re.sub(r"[^a-zA-Z0-9_]", "_", os.path.abspath(path))
  with open(path, "rb") as f:
    m = hashlib.sha256()
    m.update(f.read())
    digest = m.hexdigest()
    h = digest
    
  # make DDS
  cached_dds = cache_dir + os.sep + h + ".png"
  out = re.sub(r".png$", ".dds", path)
  if os.path.exists(cached_dds):
    print(cached_dds + " -> " + out)
    shutil.copy(cached_dds, out)
  else:
    cmd = [nvcompress, "-bc3", "-alpha", path, out]
    print(" ".join(cmd))
    subprocess.check_output(cmd)
    print(out + " -> " + cached_dds)
    shutil.copy(out, cached_dds)
  
  # make PVR
  cached_pvr = cache_dir + os.sep + h + ".pvr"
  out = re.sub(r".png$", ".pvr", path)
  if os.path.exists(cached_pvr):
    print(cached_pvr + " -> " + out)
    shutil.copy(cached_pvr, out)
  else:
    cmd = [pvrtextool, "-f", "PVRTC1_4", "-m", "-q", "pvrtcbest", "-legacypvr", "-i", path, "-o", out]
    print(" ".join(cmd))
    subprocess.check_output(cmd)
    print(out + " -> " + cached_pvr)
    shutil.copy(out, cached_pvr)
  
  # make ETC2
  # out = re.sub(r".png$", ".etc2.ktx", path)
  # cmd = [pvrtextool, "-f", "ETC2_RGBA", "-m", "-q", "etcslowperceptual", "-i", path, "-o", out]
  # print(" ".join(cmd))
  # subprocess.check_output(cmd)

def main():
  if not os.path.isdir(cache_dir):
    os.makedirs(cache_dir)
  for root, dirs, files in os.walk("."):
    for file in files:
      path = root + os.sep + file
      if file.endswith(".png"):
        cook(path)

if __name__ == "__main__":
  main()
