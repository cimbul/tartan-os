#![no_std]
#![no_main]

use core::panic::PanicInfo;

#[no_mangle]
fn efi_main() -> i32 {
    0
}

#[panic_handler]
fn panic_handler(_: &PanicInfo) -> ! {
    loop {}
}
