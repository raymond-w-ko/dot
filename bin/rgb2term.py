#! /usr/bin/env python
import colorhelper
import sys

if __name__ == '__main__':
    short, rgb = colorhelper.rgb2short(sys.argv[1])
    code = '\\[\\033[38;5;%sm\\]' % (short)
    sys.stdout.write(code)
