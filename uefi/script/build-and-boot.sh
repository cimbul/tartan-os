#!/bin/bash
#
# To boot with UEFI, we need both the bootloader and the kernel. Unfortunately, we can't
# tell Cargo that the uefi package depends on the kernel package, because:
#   * The dependency will only pull in the library, not the binary, and
#   * The kernel and bootloader have different target specs, since the UEFI environment is
#     patterned after Windows, which we don't want for Tartan OS itself.
#
# This simple script builds both packages for a given architecture and boots them.

set -e

this_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"

arch="$1"
if [ -z "$arch" ]; then
    echo "Usage: $0 ARCH" >&2
    exit 1
fi

case "$arch" in
    x86)
        kernel_target="target-specs/i686-unknown-tartan.json"
        uefi_target="target-specs/i686-unknown-uefi.json"
        ;;

    x86_64)
        kernel_target="target-specs/x86_64-unknown-tartan.json"
        uefi_target="target-specs/x86_64-unknown-uefi.json"
        ;;

    arm)
        kernel_target="target-specs/arm-unknown-tartan.json"
        uefi_target="target-specs/thumbv7a-unknown-uefi.json"
        ;;

    arm64)
        kernel_target="target-specs/aarch64-unknown-tartan.json"
        uefi_target="target-specs/aarch64-unknown-uefi.json"
        ;;
esac

# Navigate to workspace root
cd "$this_dir/../.."

(cd kernel && cargo build --target "$kernel_target")
(cd uefi && cargo run --target "$uefi_target")
