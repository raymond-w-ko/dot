#!/usr/bin/env python3
import magic
import sys
m = magic.open(magic.MIME_TYPE)
m.load()
for f in sys.argv[1:]:
    print(f, m.file(f))
