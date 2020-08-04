#![no_std]
#![cfg_attr(not(test), no_main)]
#![deny(clippy::pedantic)]

extern crate rlibc;

#[cfg(not(test))]
use core::panic::PanicInfo;
use efi::{Handle, Result, Status, SystemTable};
use utf16_lit::utf16_null;

mod efi;

#[no_mangle]
fn efi_main(image_handle: Handle, system_table: &SystemTable) -> Status {
    match main(image_handle, system_table) {
        Err(status) => status,
        Ok(status) => status,
    }
}

fn main(_image_handle: Handle, system_table: &SystemTable) -> Result {
    let hello = utf16_null!("Hello, world!\r\nWhat's up?");

    unsafe {
        let out = &*system_table.console_out;
        (out.output_string)(out, hello.as_ptr()).into_result()?;
    }

    loop { }
}

#[cfg(not(test))]
#[panic_handler]
fn panic_handler(_: &PanicInfo) -> ! {
    loop {}
}
