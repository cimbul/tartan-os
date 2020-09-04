#!/bin/bash
#
# Run all build/quality checks on the entire workspace
#

set -ex
cd "$(dirname "${BASH_SOURCE[0]}")"

cargo fmt --all -- --check
cargo clippy --workspace --all-targets -- -D warnings
cargo doc --document-private-items

cargo build --workspace
cargo test --workspace

# Cross-compiling can take a while, especially if it has to build the standard library.
# Leave this for after the tests.
for arch in x86_64 i686 aarch64 thumbv7a; do
    (cd uefi && cargo build --target target-specs/$arch-unknown-uefi.json)
done
