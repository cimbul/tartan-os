#![no_std]
#![cfg_attr(not(test), no_main)]
#![deny(clippy::pedantic)]

extern crate rlibc;

#[cfg(not(test))]
use core::panic::PanicInfo;
use core::fmt::Write;
use efi::{Handle, Result, Status, SystemTable};
use efi::proto::SimpleTextOutput;

mod efi;

// built-in writeln macro uses LF but no CR
macro_rules! writeln_cr {
    [$out:expr, $($arg:expr),*] => {
        write!($out, $($arg),*).and_then(|_| write!($out, "\r\n"))
    }
}

struct OutputStream<'a> {
    out: &'a SimpleTextOutput,
    last_result: Result,
}

impl<'a> OutputStream<'a> {
    pub fn new(out: &'a SimpleTextOutput) -> Self {
        OutputStream { out, last_result: Ok(Status::SUCCESS) }
    }
}

impl Write for OutputStream<'_> {
    fn write_str(&mut self, s: &str) -> core::fmt::Result {
        // TODO: Horribly inefficient, but simplest from a stack allocation perspective
        for c in s.chars() {
            if let e@Err(_) = self.write_char(c) {
                return e;
            }
        };
        Ok(())
    }

    fn write_char(&mut self, c: char) -> core::fmt::Result {
        // Two for UTF-16 code point (possibly a surrogate pair). One for null terminator.
        let mut buffer = [0_u16; 3];
        c.encode_utf16(&mut buffer);
        let out = &self.out;
        self.last_result = (out.output_string)(out, buffer.as_ptr()).into_result();
        match self.last_result {
            Ok(_) => Ok(()),
            Err(_) => Err(core::fmt::Error),
        }
    }
}

#[no_mangle]
fn efi_main(image_handle: Handle, system_table: &SystemTable) -> Status {
    match main(image_handle, system_table) {
        Err(status) | Ok(status) => status,
    }
}

fn main(_image_handle: Handle, system_table: &SystemTable) -> Result {
    unsafe {
        let mut out = OutputStream::new(&*system_table.console_out);

        if writeln_cr!(out, "Hello, world!\r\nWhat's up?").is_err() {
            return out.last_result;
        }
    }

    loop { }
}

#[cfg(not(test))]
#[panic_handler]
fn panic_handler(_: &PanicInfo) -> ! {
    loop {}
}
