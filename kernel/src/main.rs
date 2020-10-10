#![no_std]
#![feature(lang_items)]
#![feature(link_args)]
#![feature(start)]

#[used]
static mut X: usize = 0;

#[cfg(not(test))]
#[no_mangle]
#[start]
fn _start(_: isize, _: *const *const u8) -> isize {
    loop {
        unsafe {
            X = X.wrapping_add(1);
        }
    }
}

#[cfg(not(test))]
#[panic_handler]
fn panic_handler(_: &core::panic::PanicInfo) -> ! {
    loop {}
}

#[cfg(not(test))]
#[lang = "eh_personality"]
fn eh_personality() -> ! {
    loop {}
}

#[allow(unused_attributes)]
#[cfg_attr(all(not(test), target_os = "linux"), link_args = "-nostartfiles")]
#[cfg_attr(all(not(test), target_os = "macos"), link_args = "-lSystem")]
#[cfg_attr(
    all(target_os = "tartan", target_arch = "x86"),
    link_args = "--image-base=0x100000"
)]
#[cfg_attr(
    all(target_os = "tartan", any(target_arch = "arm", target_arch = "aarch64")),
    link_args = "--image-base=0x40000000"
)]
extern "C" {}
