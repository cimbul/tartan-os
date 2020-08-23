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
(cd uefi && cargo build)
