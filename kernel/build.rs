use std::env;

fn main() {
    let cfg_os = env::var("CARGO_CFG_TARGET_OS").unwrap();
    let cfg_arch = env::var("CARGO_CFG_TARGET_ARCH").unwrap();

    if cfg_os == "tartan" {
        match cfg_arch.as_ref() {
            "x86" => println!("cargo:rustc-link-arg=--image-base=0x100000"),
            "arm" | "aarch64" => println!("cargo:rustc-link-arg=--image-base=0x40000000"),
            _ => (),
        }
    }
}
