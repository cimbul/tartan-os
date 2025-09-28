## Rust target specs

This folder contains [custom target
specs](https://doc.rust-lang.org/rustc/targets/custom.html) for `rustc`, used to compile
the bootloader for UEFI on various architectures. Keys correspond to the
[`rustc_target::spec::TargetOps`](https://doc.rust-lang.org/nightly/nightly-rustc/rustc_target/spec/struct.TargetOptions.html) struct.

The [x86_64](./x86_64-unknown-uefi.json), [i686](./i686-unknown-uefi.json), and
[aarch64](./aarch64-unknown-uefi.json) specs are based on their built-in versions. The
only change is to `post-link-args`, enabling DWARF debug info.

The [arm-unknown-uefi](./arm-unknown-uefi.json) target is a combination of the `thumbv7a-pc-windows-msvc` and `aarch64-unknown-uefi` built-in targets, with manual
customizations to:
  * `features`
  * `llvm-floatabi`
  * `pre-link-args` (`/machine`)
