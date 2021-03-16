#!/usr/bin/env python3
import sys
import os
import subprocess
import re
import shutil

dir_path = os.path.dirname(os.path.realpath(__file__))
os.chdir(dir_path)


def yes_or_no(question):
    while "the answer is invalid":
        reply = str(input(question + " (y/n): ")).lower().strip()
        if reply[0] == "y":
            return True
        if reply[0] == "n":
            return False


def process_plugin(path):
    print(path)
    orig_dir = os.getcwd()
    os.chdir(path)

    if not os.path.exists(".git"):
        sys.exit(0)

    shutil.move(".git", "..git")
    try:
        p = subprocess.run(["git", "add", "."], check=False, capture_output=False)
        out = p.stdout.decode("utf-8")
        print(out)
    except:
        pass
    shutil.move("..git", ".git")
    os.chdir(orig_dir)


def main():
    for root, dirs, _files in os.walk("./.vim/plugged/"):
        for plugin_path in dirs:
            plugin_path = os.path.join(root, plugin_path)
            process_plugin(plugin_path)
        break
    subprocess.run(
        ["git", "commit", "-m", "vim: save plugins"], check=False, capture_output=False
    )
    print("--- EXIT ---")


if __name__ == "__main__":
    main()
