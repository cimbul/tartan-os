#!/bin/bash

set -e

this_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"
target_dir="$this_dir/../target/x86_64-unknown-uefi/debug/"
qemu_prefix="/usr/local/"

"$this_dir/make-boot-image.sh" "$target_dir/tartan-os.efi" "$target_dir/tartan-os.img"

qemu-system-x86_64 \
    -drive if=pflash,format=raw,unit=0,file="$qemu_prefix"/share/qemu/edk2-x86_64-code.fd,readonly=on \
    -drive if=ide,format=raw,file="$target_dir/tartan-os.img" \
    -net none
