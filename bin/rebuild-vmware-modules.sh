#!/bin/bash

sudo vmware-modconfig --console --install-all
sudo systemctl restart vmware-usbarbitrator.service
sudo systemctl restart vmware.service
