#!/bin/bash

set -e

this_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"

mkdir -p "$this_dir/dtb"

for source_file in "$this_dir"/dts/*.dts; do
    blob_file="$this_dir/dtb/$(basename "$source_file" ".dts").dtb"

    # Devicetree compiler developed for the linux kernel
    # https://git.kernel.org/pub/scm/utils/dtc/dtc.git
    dtc "$source_file" > "$blob_file"
done
