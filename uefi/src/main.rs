#![no_std]
#![cfg_attr(not(test), no_main)]
#![cfg_attr(not(test), feature(lang_items))]
#![feature(alloc_error_handler)]
#![feature(panic_info_message)]
#![deny(clippy::pedantic)]
#![allow(clippy::must_use_candidate)]

extern crate rlibc;
extern crate alloc;

#[cfg(not(test))]
use core::panic::PanicInfo;
#[cfg(not(test))]
use tartan_uefi::allocator::BootAllocator;
#[cfg(not(test))]
use core::alloc::Layout;

use alloc::string::String;
use core::fmt::Write;
use tartan_uefi::{BootServices, Handle, MemoryMap, Result, Status, SystemTable, Table};
use tartan_uefi::proto::{LoadedImage, Protocol};
use tartan_uefi::io::OutputStream;
use tartan_uefi::global::SYSTEM_TABLE;
use tartan_uefi::writeln_result;

#[no_mangle]
fn efi_main(image_handle: Handle, system_table: &'static mut SystemTable) -> Status {
    unsafe {
        SYSTEM_TABLE = Some(system_table);
    }

    match efi_main_result(image_handle, system_table) {
        Err(status) | Ok(status) => status,
    }
}

fn efi_main_result(image_handle: Handle, system_table: &mut SystemTable) -> Result {
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
            &mut (loaded_image.cast()),
        ).into_result()?;

        let image_base = (*loaded_image).image_base;
        writeln_result!(out, "Image base: {:p}", image_base)?;

        writeln_result!(out, "Attempting heap allocation")?;
        writeln_result!(out, "Testing... {}", String::from("Success!"))?;

        writeln_result!(out, "Fetching memory map")?;
        let memory_map = get_memory_map(boot_services)?;
        writeln_result!(out,
            "Got memory map: size = {} bytes ({} per descriptor), version {}, key = {}",
            memory_map.raw_map.len(),
            memory_map.descriptor_size,
            memory_map.descriptor_version,
            memory_map.key
        )?;
        for (i, descriptor) in memory_map.iter().enumerate() {
            writeln_result!(out,
                "Region {}: {:x} => {:x} + {:3x} pages, {:?}",
                i,
                descriptor.physical_start,
                descriptor.virtual_start,
                descriptor.page_count,
                descriptor.memory_type
            )?;
        }
    }

    loop { }
}

fn get_memory_map(boot_services: &BootServices) -> core::result::Result<MemoryMap, Status> {
    let mut memory_map_size = 0_usize;
    let mut memory_map = MemoryMap::new();

    loop {
        memory_map.raw_map.resize(memory_map_size, 0);

        let result = unsafe {
            (boot_services.get_memory_map)(
                &mut memory_map_size,
                // TODO: Make sure this is aligned properly. memory_map.verify() will
                // check and panic if it isn't, but we should be able to ensure it.
                memory_map.raw_map.as_mut_ptr().cast(),
                &mut memory_map.key,
                &mut memory_map.descriptor_size,
                &mut memory_map.descriptor_version,
            ).into_result()
        };
        match result {
            Ok(_) => break,
            Err(Status::BUFFER_TOO_SMALL) =>
                // Allow room for another entry since we have to reallocate the buffer
                memory_map_size += memory_map.descriptor_size,
            Err(e) => return Err(e),
        }
    };

    // Trim anything that wasn't used
    memory_map.raw_map.resize(memory_map_size, 0);

    memory_map.verify();
    Ok(memory_map)
}

#[cfg(not(test))]
#[panic_handler]
fn panic_handler(info: &PanicInfo) -> ! {
    // Not much we can do with Err results in a panic handler
    #[allow(unused_must_use)]

    unsafe {
        if let Some(system_table) = SYSTEM_TABLE {
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

#[cfg(not(test))]
#[global_allocator]
static BOOT_ALLOCATOR: BootAllocator = BootAllocator;

#[cfg(not(test))]
#[alloc_error_handler]
fn alloc_error(layout: Layout) -> ! {
    panic!("Allocating {} bytes failed", layout.size());
}

// Hack to get the binary to build on the host target.
#[cfg(not(test))]
#[lang = "eh_personality"]
fn eh_personality() -> ! {
    loop {}
}

// Hack to get the binary to build on the host target. It obviously doesn't do anything.
#[cfg(not(test))]
#[no_mangle]
fn main(_: isize, _: *const *const u8) -> isize { 100 }
