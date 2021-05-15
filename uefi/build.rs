use std::env;

fn main() {
    let cfg_test = env::var("CARGO_CFG_TEST").is_ok();
    let cfg_os = env::var("CARGO_CFG_TARGET_OS").unwrap();

    if !cfg_test {
        // Special args needed to build on the host system
        match cfg_os.as_ref() {
            "linux" => println!("cargo:rustc-link-arg=-nostartfiles"),
            "macos" => println!("cargo:rustc-link-arg=-lSystem"),
            _ => (),
        }
    }
}
