#!/bin/bash

set -e
sudo vmware-modconfig --console --install-all
set +e
sudo systemctl restart vmware-usbarbitrator.service
sudo systemctl restart vmware.service
