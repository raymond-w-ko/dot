#!/usr/bin/env python2

import gnomekeyring as gk

for keyring in ['login', 'Default']:
    for keyItem in gk.list_item_ids_sync(keyring):
        key = gk.item_get_info_sync(keyring, keyItem)
        attr = gk.item_get_attributes_sync(keyring, keyItem)
        print(key.get_display_name(), attr)
