#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$DIR/.vim/bundle/"
find . \( -type f -or -type l \) \
  ! -path '*/.git/*' \
  ! -path '*/.cpcache/*' \
  ! -path '*/server/geckocomplete/target/*' \
  ! -name '*.pyc' \
  ! -name '*.sock' \
  ! -name '*.so' \
  ! -name '.nrepl-port' \
  ! -name '*.o' \
  ! -name '*.dll' \
  ! -name '*.exe' \
  ! -name '*.class' \
  -exec git add -f --verbose {} \;
git commit -m "updated vim bundle(s)" .
