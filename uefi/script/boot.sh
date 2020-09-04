#!/bin/bash

set -e

qemu_prefix="/usr/local/"

arch="$1"
executable="$2"

if [ -z "$arch" ] || [ -z "$executable" ]; then
    echo "USAGE: $0 ARCH EXECUTABLE_FILE" >&2
    exit 1
fi

target_dir="$(dirname "$executable")"

case "$arch" in
    x86)
        boot_filename="BOOTIA32.EFI"
        qemu_suffix="i386"
        efi_code="edk2-i386-code.fd"
        efi_vars="edk2-i386-vars.fd"
        qemu_args=()
        ;;
    x86-64)
        boot_filename="BOOTX64.EFI"
        qemu_suffix="x86_64"
        efi_code="edk2-x86_64-code.fd"
        efi_vars="edk2-i386-vars.fd"  # Shared with 32-bit
        qemu_args=()
        ;;
    arm)
        # FIXME: 32-bit Arm currently doesn't boot because UEFI expects the PE machine
        # type to be 0x01c2 (Thumb), but LLVM only emits 0x01c4 (Arm "NT", whatever that
        # means) for 32-bit targets, and 0xaa64 (AArch64) for 64-bit.
        boot_filename="BOOTARM.EFI"
        qemu_suffix="arm"
        efi_code="edk2-arm-code.fd"
        efi_vars="edk2-arm-vars.fd"
        qemu_args=(
            -machine virt
            -cpu cortex-a15
        )
        ;;
    arm64)
        boot_filename="BOOTAA64.EFI"
        qemu_suffix="aarch64"
        efi_code="edk2-aarch64-code.fd"
        efi_vars="edk2-arm-vars.fd"  # Shared with 32-bit
        qemu_args=(
            -machine virt
            -cpu cortex-a57
        )
        ;;
    *)
        echo "Unknown architecture '$arch'. Cannot boot with UEFI." >&2
        exit 1
esac

# Prepare directory for virtual FAT boot image
boot_dir="$target_dir/boot-fs"
mkdir -p "$boot_dir/EFI/BOOT"
cp "$executable" "$boot_dir/EFI/BOOT/$boot_filename"

# Copy EFI var file to allow writes (required by EDK2 on ARM)
cp "$qemu_prefix"/share/qemu/"$efi_vars" "$target_dir"/"$efi_vars"

cat >&2 <<EOF
=============================
Starting QEMU in console mode
 > To exit, press Ctrl+a, x
 > For more, press Ctrl+a, h
=============================
EOF
qemu-system-"$qemu_suffix" \
    "${qemu_args[@]}" \
    -drive if=pflash,format=raw,unit=0,file="$qemu_prefix"/share/qemu/"$efi_code",readonly=on \
    -drive if=pflash,format=raw,unit=1,file="$target_dir"/"$efi_vars" \
    -drive if=virtio,id=stick,format=raw,file=fat:rw:"$boot_dir" \
    -nographic \
    -net none
