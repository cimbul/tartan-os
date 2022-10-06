# Tartan OS

[![GitHub Workflow Status](https://img.shields.io/github/workflow/status/cimbul/tartan-os/Build)](https://github.com/cimbul/tartan-os/actions?query=workflow%3ABuild)
[![License](https://img.shields.io/github/license/cimbul/tartan-os)](./LICENSE)

A toy OS written in Rust. The name is a reference to the old logo of a certain rust
removal product.

So far, it is not much more than a UEFI bootloader with some in-progress support for ACPI
(including an AML parser and interpreter).

This is mainly a personal learning project to get familiar with Rust, as well as to update
my knowledge about OS internals and hardware that's newer than the PC/AT. 🙃


## Dependencies

Tested on macOS and Linux.

  * Rust 1.66.0 _nightly_ toolchain
  * [QEMU](https://www.qemu.org/) 5.1
  * bash, for the QEMU launcher script
  * Python 3.2+ for booting 32-bit Arm only

In order to edit the ACPI parser integration tests, you will also need tools documented
at [acpi/tests/parse/README.md](acpi/tests/parse/).

Suggested additional tools:
  * [`cargo-binutils`](https://github.com/rust-embedded/cargo-binutils)


## Build

Note that when building, running, or testing with Cargo, the build settings will *depend
on your current directory* due to the provided [Cargo configuration
files](https://doc.rust-lang.org/cargo/reference/config.html):
  * From the main directory, Cargo will build for your host system by default, using the
    prebuilt standard Rust libraries from your toolchain.
  * From the `uefi/` directory, Cargo will compile the standard rust libraries for the
    specified `--target`, which is `x86_64-unknown-uefi` by default. This is configured in
    [`uefi/.cargo/config.toml`](uefi/.cargo/config.toml).

### Libraries for host system

```bash
cargo build
```

### Kernel for target system

To build the kernel for the target system:

```bash
(cd kernel && cargo build --target target-specs/<ARCH>-unknown-kernel.json)

# Or alternatively, from outside the kernel/ directory:
cargo build --package tartan-kernel -Z build-std \
    --target kernel/target-specs/<ARCH>-unknown-kernel.json
```

Where `<ARCH>` is one of:
  * `x86_64`
  * `i686`
  * `aarch64`
  * `arm`

### UEFI bootloader for target system

To build the UEFI bootloader for the target system:

```bash
(cd uefi && cargo build --target target-specs/<ARCH>-unknown-uefi.json)

# Or alternatively, from outside the uefi/ directory:
cargo build --package tartan-uefi -Z build-std \
    --target uefi/target-specs/<ARCH>-unknown-uefi.json
```

The UEFI application (a PE image) will be emitted at
`target/<ARCH>-unknown-uefi/debug/tartan-uefi.efi`.

Note that for the `arm` target, the PE image does not have the "machine type" expected by
the UEFI runtime, so it will not boot as-is. This is fixed with a Python script when
booting via `cargo run` (see below), but you will have to manually fix it if you deploy
the application yourself. See [`uefi/script/boot.sh`](uefi/script/boot.sh).


## Run in QEMU

```bash
./uefi/script/build-and-boot.sh <ARCH>
```

This builds the kernel and UEFI bootloader and then launches the UEFI application in QEMU
using [`uefi/script/boot.sh`](uefi/script/boot.sh).


## Test

```bash
cargo test
```

Runs tests on your host system. There is currently no support for running the tests on the
target architecture in QEMU.


## Documentation

```bash
cargo doc --open
```

Documentation from the latest build is available at https://tartan.cimbul.com/.


## Lint

```bash
cargo clippy --all-targets
```


## License

© Copyright 2020 Tim Yates

> This program is free software: you can redistribute it and/or modify
> it under the terms of the GNU General Public License as published by
> the Free Software Foundation, either version 3 of the License, or
> (at your option) any later version.
>
> This program is distributed in the hope that it will be useful,
> but WITHOUT ANY WARRANTY; without even the implied warranty of
> MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
> GNU General Public License for more details.
>
> You should have received a copy of the GNU General Public License
> along with this program.  If not, see <https://www.gnu.org/licenses/>.

I would be willing to release this under a more permissive license if someone finds it
useful, but I don't expect that to be the case.
