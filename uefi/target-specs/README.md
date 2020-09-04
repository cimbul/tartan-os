## Rust target specs

This folder contains [custom target
specs](https://doc.rust-lang.org/rustc/targets/custom.html) for `rustc`, used to compile
the bootloader for UEFI on various architectures. Keys correspond to the
[`rustc_target::spec::TargetOps`](https://doc.rust-lang.org/nightly/nightly-rustc/rustc_target/spec/struct.TargetOptions.html) struct.

The [x86_64](./x86_64-unknown-uefi.json) and [i686](./i686-unknown-uefi.json) specs are
built-in, but they are included here for completeness.
