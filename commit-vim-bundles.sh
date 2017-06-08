#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$DIR/.vim/bundle/"
find . \( -type f -or -type l \) \
  ! -path '*/.git/*' \
  ! -name '*.pyc' \
  ! -name '*.so' \
  ! -name '*.o' \
  ! -name '*.dll' \
  ! -name '*.exe' \
  -exec git add -f --verbose {} \;
git commit -m "updated vim bundle(s)" .
