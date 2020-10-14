#![no_std]
#![feature(lang_items)]
#![feature(link_args)]
#![feature(rustc_private)]
#![feature(start)]

use tartan_serial::{LineMode, UART};


mod intrinsics;


#[used]
static mut X: usize = 0;

#[no_mangle]
#[cfg_attr(not(test), start)]
fn _start(_: isize, _: *const *const u8) -> isize {
    let mut serial = find_uart();
    serial.reset();
    serial.set_line_mode(LineMode::default());
    serial.write(b"Hello, world!\r\n");

    loop {
        unsafe {
            X = X.wrapping_add(1);
        }
    }
}


#[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
fn find_uart() -> impl UART {
    tartan_serial::model_16550::UART16550::new(0x3f8)
}

#[cfg(any(target_arch = "arm", target_arch = "aarch64"))]
fn find_uart() -> impl UART {
    tartan_serial::NullUART
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
