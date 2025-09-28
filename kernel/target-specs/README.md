## Rust target specs

This folder contains [custom target
specs](https://doc.rust-lang.org/rustc/targets/custom.html) for `rustc`, used to compile
the kernel on various architectures. Keys correspond to the
[`rustc_target::spec::TargetOps`](https://doc.rust-lang.org/nightly/nightly-rustc/rustc_target/spec/struct.TargetOptions.html) struct.

They are based on these built-in targets:
  * [aarch64-unknown-tartan](./aarch64-unknown-tartan.json): `aarch64-unknown-none`
  * [arm-unknown-tartan](./arm-unknown-tartan.json): `armv7a-none-eabi`
  * [i686-unknown-tartan](./i686-unknown-tartan.json):
    * Mostly `x86_64-unknown-none`
    * 32-bit adaptations copied from `i686-unknown-linux-gnu`
    * Disabled position independent code to avoid errors
  * [x86_64-unknown-tartan](./x86_64-unknown-tartan.json): `x86_64-unknown-none`
    * Disabled position independent code to avoid errors

The primary changes from these built-in targets are:
 * `os`
 * `target-family`
 * `vendor`
 * `metadata`
