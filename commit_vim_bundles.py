#!/usr/bin/env python3
import subprocess
import os
import re

dir_path = os.path.dirname(os.path.realpath(__file__))
os.chdir(dir_path)


def yes_or_no(question):
    while "the answer is invalid":
        reply = str(input(question + " (y/n): ")).lower().strip()
        if reply[0] == "y":
            return True
        if reply[0] == "n":
            return False


p = subprocess.run(["git", "status"], check=True, capture_output=True)

files = []
lines = p.stdout.decode("utf-8").split("\n")
for line in lines:
    m = re.match(r"^\s*modified:\s*(.+)$", line)
    if not m:
        continue
    x = m.group(1)
    if ".vim/bundle/" not in x:
        continue
    files.append(x)

for x in files:
    print(x)
if yes_or_no("git add?"):
    for x in files:
        subprocess.run(["git", "add", x], check=True)
    subprocess.run(["git", "commit", "-m", "updated vim bundle(s)"], check=True)
