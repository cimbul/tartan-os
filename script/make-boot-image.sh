#!/bin/bash

set -e

uefi_app="$1"
hd_img="$2"
if [ -z "$uefi_app" ] || [ -z "$hd_img" ]; then
    echo "USAGE: $0 INPUT.EFI OUTPUT.IMG" >&2
    exit 1
fi

boot_dir="$(mktemp -d 2>/dev/null || mktemp -d -t 'boot_dir')"
mkdir -p "$boot_dir/EFI/BOOT"
cp "$uefi_app" "$boot_dir/EFI/BOOT/BOOTX64.EFI"

hdiutil create \
    -ov \
    -fs fat32 \
    -size 64m \
    -format UDTO \
    -srcfolder "$boot_dir" \
    "$hd_img.cdr"
mv "$hd_img.cdr" "$hd_img"
