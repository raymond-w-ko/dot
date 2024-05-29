#!/bin/bash
ps uxa | grep .vscode-server | grep -v grep | awk '{print $2}' | xargs kill
