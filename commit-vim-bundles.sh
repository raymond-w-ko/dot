#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$DIR"
find . \( -type f -or -type l \) \
  ! -path '*/.git/*' \
  ! -name '*.pyc' \
  ! -name '*.so' \
  ! -name '*.o' \
  ! -name '*.dll' \
  -exec git add -f {} \;
