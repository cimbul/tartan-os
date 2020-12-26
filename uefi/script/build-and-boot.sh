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

# Navigate to workspace root
cd "$this_dir/../.."

(cd kernel && cargo build --target "target-specs/$arch-unknown-tartan.json")
(cd uefi && cargo run --target "target-specs/$arch-unknown-uefi.json")
