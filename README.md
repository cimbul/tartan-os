# Tartan OS

![GitHub Workflow Status](https://img.shields.io/github/workflow/status/cimbul/tartan-os/Build)
![License](https://img.shields.io/github/license/cimbul/tartan-os)

A toy OS written in Rust. The name is a reference to the old logo of a certain rust
removal product.

So far, it is not much more than a UEFI bootloader with some in-progress support for ACPI
(including an AML parser and interpreter).

This is mainly a personal learning project to get familiar with Rust, as well as to update
my knowledge about OS internals and hardware that's newer than the PC/AT. 🙃


## Dependencies

Tested on macOS and Linux.

  * Rust 1.48.0 _nightly_ toolchain, for better cross-compilation features in Cargo
  * [QEMU](https://www.qemu.org/) 5.0
  * bash, for the UEFI launcher script

In order to edit the ACPI parser integration tests, you will also need tools documented
at [acpi/tests/parse/README.md](acpi/tests/parse/).

Suggested additional tools:
  * [`cargo-binutils`](https://github.com/rust-embedded/cargo-binutils)


## Build

Note that when building, running, or testing with Cargo, the default target will *depend
on your current directory*:
  * From the main directory, Cargo will build for your host system by default.
  * From the `uefi/` directory, Cargo will build for the `x86_64-unknown-uefi` target,
    along with flags to compile the standard Rust libraries. This is configured in
    `uefi/.cargo/config.toml`.

To build the libraries for your host system:

```bash
cargo build
```

To build the AMD64 UEFI bootloader:

```bash
(cd uefi && cargo build)
```

The UEFI application (a PE image) will be emitted at
`target/x86_64-unknown-uefi/debug/tartan-uefi.efi`.


## Run UEFI bootloader

```bash
(cd uefi && cargo run)
```

This launches the UEFI application in QEMU using
[`uefi/script/boot.sh`](uefi/script/boot.sh).


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
