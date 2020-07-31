#![no_std]
#![no_main]

use core::panic::PanicInfo;
use efi::{Handle, SystemTable};
use utf16_lit::utf16_null;

mod efi;

#[no_mangle]
fn efi_main(_image_handle: Handle, system_table: *const SystemTable) -> ! {
    let hello = utf16_null!("Hello, world!\r\nWhat's up?");

    unsafe {
        let out = (*system_table).console_out;
        let output_string = (*out).OutputString;
        output_string(out, hello.as_ptr());
    }

    loop { }
}

#[panic_handler]
fn panic_handler(_: &PanicInfo) -> ! {
    loop {}
}
