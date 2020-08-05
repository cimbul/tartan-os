#![no_std]
#![cfg_attr(not(test), no_main)]
#![feature(panic_info_message)]
#![feature(ptr_offset_from)]
#![deny(clippy::pedantic)]

extern crate rlibc;

#[cfg(not(test))]
use core::panic::PanicInfo;
use core::ffi::c_void;
use core::fmt::Write;
use efi::{Handle, Result, Status, SystemTable, Table};
use efi::proto::{LoadedImage, Protocol, SimpleTextOutput};

static mut SYSTEM_TABLE_STATIC: Option<*mut SystemTable> = None;

mod efi;

macro_rules! writeln_result {
    [$out:ident, $($args:expr),*] => {
        match writeln!($out, $($args),*) {
            _ => $out.last_result
        }
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
        // Automatically translate LF to CRLF. Internally-used panic strings can contain
        // line breaks (e.g., assert_eq!), and this ensures they are formatted correctly.
        if c == '\n' {
            self.write_char('\r')?;
        }

        // Two for UTF-16 code point (possibly a surrogate pair). One for null terminator.
        let mut buffer = [0_u16; 3];
        c.encode_utf16(&mut buffer);
        let out = &self.out;
        unsafe {
            self.last_result = (out.output_string)(out, buffer.as_ptr()).into_result();
        }
        match self.last_result {
            Ok(_) => Ok(()),
            Err(_) => Err(core::fmt::Error),
        }
    }
}

#[no_mangle]
fn efi_main(image_handle: Handle, system_table: &'static mut SystemTable) -> Status {
    unsafe {
        SYSTEM_TABLE_STATIC = Some(system_table);
    }

    match main(image_handle, system_table) {
        Err(status) | Ok(status) => status,
    }
}

fn main(image_handle: Handle, system_table: &mut SystemTable) -> Result {
    unsafe {
        let mut out = OutputStream::new(&*system_table.console_out.unwrap());

        writeln_result!(out, "Hello, world!\nWhat's up?")?;

        writeln_result!(out, "Verifying system tables...")?;
        system_table.verify();
        system_table.runtime_services.verify();
        system_table.boot_services.unwrap().verify();
        writeln_result!(out, "Verified!")?;

        let loaded_image: *const LoadedImage = core::ptr::null();
        let boot_services = system_table.boot_services.unwrap();
        (boot_services.handle_protocol)(
            image_handle,
            &LoadedImage::PROTOCOL_ID,
            &mut (loaded_image as *const c_void),
        ).into_result()?;

        let image_base = (*loaded_image).image_base;
        writeln_result!(out, "Image base: {:p}", image_base)?;
    }

    loop { }
}

#[cfg(not(test))]
#[panic_handler]
fn panic_handler(info: &PanicInfo) -> ! {
    // Not much we can do with Err results in a panic handler
    #[allow(unused_must_use)]

    unsafe {
        if let Some(system_table) = SYSTEM_TABLE_STATIC {
            if let Some(console_out) = (*system_table).console_out {
                let mut out = OutputStream::new(console_out);
                writeln!(out, "!!! Panic !!!");
                match info.location() {
                    Some(location) => writeln!(out, "Location: {}", location),
                    None => writeln!(out, "No location information"),
                };
                match info.message() {
                    Some(arguments) => core::fmt::write(&mut out, *arguments),
                    None => writeln!(out, "No additional message"),
                };
            }
        }
    }

    loop {}
}
