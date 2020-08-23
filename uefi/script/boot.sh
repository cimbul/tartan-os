#!/bin/bash

set -e

qemu_prefix="/usr/local/"

executable="$1"
if [ -z "$executable" ]; then
    echo "USAGE: $0 EXECUTABLE_FILE" >&2
    exit 1
fi
target_dir="$(dirname "$executable")"

# Prepare directory for virtual FAT boot image
boot_dir="$target_dir/boot-fs"
mkdir -p "$boot_dir/EFI/BOOT"
cp "$target_dir/tartan-uefi.efi" "$boot_dir/EFI/BOOT/BOOTX64.EFI"

cat >&2 <<EOF
=============================
Starting QEMU in console mode
 > To exit, press Ctrl+a, x
 > For more, press Ctrl+a, h
=============================
EOF
qemu-system-x86_64 \
    -drive if=pflash,format=raw,unit=0,file="$qemu_prefix"/share/qemu/edk2-x86_64-code.fd,readonly=on \
    -drive if=ide,format=raw,file=fat:rw:"$boot_dir" \
    -nographic \
    -net none
