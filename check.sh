#!/bin/bash
#
# Run all build/quality checks on the entire workspace
#

set -ex
cd "$(dirname "${BASH_SOURCE[0]}")"

cargo fmt --all -- --check
cargo clippy --workspace --all-targets -- -D warnings
cargo doc --document-private-items

cargo test --workspace

# Cross-compiling can take a while, especially if it has to build the standard library.
# Leave this for after the tests.
archs="x86_64 i686 aarch64 arm"
for arch in $archs; do
    (cd kernel && cargo build --target "target-specs/$arch-unknown-tartan.json")
done
for arch in $archs; do
    (cd uefi && cargo build --target "target-specs/$arch-unknown-uefi.json")
done
