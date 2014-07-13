#!/bin/bash

set -e

ORIGDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

cd /etc/openvpn/

sudo rm -f ca.crt
sudo ln -s "$ORIGDIR/ca.crt"

sudo rm -f Wdc.key
sudo ln -s "$ORIGDIR/Wdc.key"

sudo rm -f passwd
sudo ln -s "$HOME/.ssh/purevpn" passwd

sudo rm -f client.conf
sudo ln -s "$ORIGDIR/UDP/USA-Chicago-UDP.ovpn" client.conf

ls -l --color=yes /etc/openvpn

[[ -f /etc/openvpn/client.conf ]] && sudo systemctl restart openvpn@client.service
