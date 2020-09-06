#!/bin/bash

set -e

this_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"

arch="$1"
executable="$2"

if [ -z "$arch" ] || [ -z "$executable" ]; then
    echo "USAGE: $0 ARCH EXECUTABLE_FILE" >&2
    exit 1
fi

case "$arch" in
    x86)
        extra_offset="0"
        ;;
    x86-64)
        extra_offset="0"
        ;;
    arm)
        # For some bizarre reason, the debug info is off by one for 32-bit Arm
        extra_offset="-1"
        # GDB chokes on Arm PE files (both 32-bit and 64-bit). Setting the machine type to
        # x86/x86-64 tricks it into doing the right thing. It still gets the correct
        # architecture from the remote server (QEMU).
        debug_machine_type="0x014c"  # x86
        ;;
    arm64)
        extra_offset="0"
        debug_machine_type="0x8664"  # x86-64
        ;;
    *)
        echo "Unknown architecture '$arch'. Cannot set up debugger." >&2
        exit 1
esac


# Determine the offset we need to point the debug info to the location the image was
# loaded at runtime.

file_base_addr=$("$this_dir"/get-pe-image-base.py "$executable")
if [ -z "$file_base_addr" ]; then
    echo "Could not find base address in file $executable" >&2
    exit 2
fi
echo "Base address from file: $file_base_addr"

read -r -p "Runtime base address (see QEMU serial console): " runtime_base_addr

offset=$((runtime_base_addr - file_base_addr + extra_offset))
offset=$(printf "%#x" "$offset")
echo "Loading with offset $offset"


# If necessary, change the PE machine type to make it readable by GDB
if [ -n "$debug_machine_type" ]; then
    executable_debug="$executable.debug"
    cp "$executable" "$executable_debug"
    "$this_dir"/set-pe-machine-type.py "$executable_debug" "$debug_machine_type"
else
    executable_debug="$executable"
fi


# Start GDB and connect to QEMU with the symbols loaded in the right place
rust-gdb \
    -ex "symbol-file $executable_debug -o $offset" \
    -ex "target remote :1234"
