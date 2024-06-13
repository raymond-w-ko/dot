#!/bin/bash
ps uxa | grep .vscode-server | grep -v grep | grep -v killall-vscode-server.sh | awk '{print $2}' | xargs kill
