#!/bin/bash

find . \( -type f -or -type l \) ! -path '*/.git/*' ! -name '*.pyc' -exec git add -f {} \;
