#!/bin/bash

KERNEL_VERSION=$(uname -r | cut -c-3)
SIGN=/usr/lib/linux-kbuild-${KERNEL_VERSION}/scripts/sign-file
cd /lib/modules/$(uname -r)/updates/dkms

for ko in $(ls *.ko); do
    if [ -f ${ko} ]; then
        echo "Signing ${ko}"
        sudo ${SIGN} sha256 ~/MOK.priv ~/MOK.der ${ko}
    fi
done
