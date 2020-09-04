#![no_std]
#![cfg_attr(not(test), no_main)]
#![cfg_attr(not(test), feature(lang_items))]
#![cfg_attr(not(test), feature(link_args))]
#![feature(alloc_error_handler)]
#![feature(asm)]
#![feature(naked_functions)]
#![feature(panic_info_message)]
#![warn(clippy::pedantic)]
#![allow(clippy::must_use_candidate)]

extern crate alloc;
extern crate rlibc;

#[cfg(not(test))]
use core::alloc::Layout;
#[cfg(not(test))]
use core::panic::PanicInfo;
#[cfg(not(test))]
use tartan_uefi::allocator::BootAllocator;

use alloc::string::String;
use core::fmt::Write;
use log::info;
use tartan_uefi::global::SYSTEM_TABLE;
use tartan_uefi::io::{Logger, OutputStream};
use tartan_uefi::proto::{LoadedImage, Protocol};
use tartan_uefi::writeln_result;
use tartan_uefi::{
    BootServices, Handle, MemoryMap, OpenProtocolAttributes, Result, Status, SystemTable,
    Table,
};


static mut LOGGER: Logger = Logger(None);


#[no_mangle]
fn efi_main(image_handle: Handle, system_table: &'static mut SystemTable) -> Status {
    unsafe {
        SYSTEM_TABLE = Some(system_table);

        LOGGER.0 = Some(OutputStream::new(&*system_table.console_out.unwrap()));
        log::set_logger(&LOGGER).unwrap();
        log::set_max_level(log::LevelFilter::max());
    }

    match efi_main_result(image_handle, system_table) {
        Err(status) | Ok(status) => status,
    }
}

fn efi_main_result(image_handle: Handle, system_table: &mut SystemTable) -> Result {
    unsafe {
        let mut out = OutputStream::new(&*system_table.console_out.unwrap());
        writeln_result!(out, "Hello, world!\nWhat's up?")?;

        info!("Logging initialized");

        writeln_result!(out, "Verifying system tables...")?;
        system_table.verify();
        system_table.runtime_services.verify();
        system_table.boot_services.unwrap().verify();
        writeln_result!(out, "Verified!")?;

        let mut loaded_image: *const LoadedImage = core::ptr::null();
        let boot_services = system_table.boot_services.unwrap();
        (boot_services.open_protocol)(
            image_handle,
            &LoadedImage::PROTOCOL_ID,
            ((&mut loaded_image) as *mut *const LoadedImage).cast(),
            image_handle,
            Handle::NULL,
            OpenProtocolAttributes::GET,
        )
        .into_result()?;

        assert_ne!(loaded_image, core::ptr::null());
        let image_base = (*loaded_image).image_base;
        writeln_result!(out, "Image base: {:p}", image_base)?;

        writeln_result!(out, "Attempting heap allocation")?;
        writeln_result!(out, "Testing... {}", String::from("Success!"))?;

        writeln_result!(out, "Fetching memory map")?;
        let memory_map = get_memory_map(boot_services)?;
        writeln_result!(
            out,
            "Got memory map: size = {} bytes ({} per descriptor), version {}, key = {}",
            memory_map.raw_map.len(),
            memory_map.descriptor_size,
            memory_map.descriptor_version,
            memory_map.key
        )?;
        for (i, descriptor) in memory_map.iter().enumerate() {
            writeln_result!(
                out,
                "Region {}: {:x} => {:x} + {:3x} pages, {:?}",
                i,
                descriptor.physical_start,
                descriptor.virtual_start,
                descriptor.page_count,
                descriptor.memory_type
            )?;
        }

        enumerate_pci(&mut out)?;
    }

    loop {}
}

#[cfg(not(any(arch = "x86", arch = "x86_64")))]
fn enumerate_pci(_: &mut OutputStream) -> Result {
    // TODO
    Ok(Status::SUCCESS)
}

#[cfg(any(arch = "x86", arch = "x86_64"))]
fn enumerate_pci(out: &mut OutputStream) -> Result {
    use tartan_pci as pci;
    use tartan_pci::access::{io::IOConfigAccess, ConfigAccess, ConfigSelector};

    writeln_result!(out, "Enumerating PCI devices on bus 0")?;
    let access = IOConfigAccess;
    for selector in pci::enumerate_bus(&access, ConfigSelector::default()) {
        let id_register: pci::config::HeaderRegister0 =
            access.get_fixed_register(selector);

        writeln_result!(
            out,
            " {:2x}:{:x}: vendor {:04x} device {:04x}",
            selector.device,
            selector.function,
            id_register.vendor(),
            id_register.device(),
        )?;

        let register_3: pci::config::HeaderRegister3 =
            access.get_fixed_register(selector);
        let function_count_note = if register_3.header_type().multi_function() {
            "multi-function"
        } else {
            "single-function"
        };
        writeln_result!(
            out,
            "       header type {:02x} ({})",
            register_3.header_type().header_type(),
            function_count_note,
        )?;


        let class_register: pci::config::HeaderRegister2 =
            access.get_fixed_register(selector);
        writeln_result!(
            out,
            "       class {:02x} subclass {:02x} interface {:02x} revision {:02x}",
            class_register.class(),
            class_register.subclass(),
            class_register.interface(),
            class_register.revision(),
        )?;
    }

    Ok(Status::SUCCESS)
}

fn get_memory_map(
    boot_services: &BootServices,
) -> core::result::Result<MemoryMap, Status> {
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
            )
            .into_result()
        };
        match result {
            Ok(_) => break,
            Err(Status::BUFFER_TOO_SMALL) => {
                // Allow room for another entry since we have to reallocate the buffer
                memory_map_size += memory_map.descriptor_size
            }
            Err(e) => return Err(e),
        }
    }

    // Trim anything that wasn't used
    memory_map.raw_map.resize(memory_map_size, 0);

    memory_map.verify();
    Ok(memory_map)
}

#[cfg(not(test))]
#[panic_handler]
fn panic_handler(info: &PanicInfo) -> ! {
    // Not much we can do with Err results in a panic handler
    #![allow(unused_must_use)]

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
#[cfg(not(any(test, target_os = "uefi")))]
#[no_mangle]
fn main(_: isize, _: *const *const u8) -> isize {
    100
}

// More hacks to get the binary to build on the host target.
// Thanks to https://fasterthanli.me/series/making-our-own-executable-packer/part-12
// for showing how to do this without breaking dependencies' build scripts.
#[allow(unused_attributes)]
#[cfg_attr(all(not(test), target_os = "linux"), link_args = "-nostartfiles")]
#[cfg_attr(all(not(test), target_os = "macos"), link_args = "-lSystem")]
extern "C" {}

// compiler-builtins doesn't mangle this name correctly for Windows's cdecl convention on
// x86, which adds a leading underscore. It would be better to alias this symbol directly,
// but I can't get LLVM ASM to handle that right.
#[cfg(all(target_os = "uefi", target_arch = "x86"))]
#[no_mangle]
#[naked]
pub unsafe fn __rust_probestack() {
    // This looks recursive, but isn't. This function is ___rust_probestack (triple).
    asm!("jmp __rust_probestack");
}

/// This is a Microsoft C Runtime Library function that LLVM expects to be available when
/// it is making PE files for Arm.
#[cfg(all(target_os = "uefi", target_arch = "aarch64"))]
#[no_mangle]
#[naked]
pub unsafe extern "C" fn __chkstk() {
    // Input:    x15 = number of 16-byte units in stack
    // Output:   x15 = same as input
    // Clobbers: x16, x17
    asm!(
        "
        mov  x16, 0    // Stack offset
        mov  x17, x15  // Remaining 16-byte units

    1:  // Repeatedly touch the guard page to trigger faults and allocate more stack
        sub  x16, x16, 4096   // Page size in bytes
        ldr  xzr, [sp, x16]
        subs x17, x17, 256    // Page size in 16-byte units
        b.gt 1b

        ret
        "
    )
}

/// This is a Microsoft C Runtime Library function that LLVM expects to be available when
/// it is making PE files for Arm.
#[cfg(all(target_os = "uefi", target_arch = "arm"))]
#[no_mangle]
#[naked]
pub unsafe extern "C" fn __chkstk() {
    // Input:    r4 = number of 4-byte units in stack
    // Output:   r4 = number of *individual* bytes in stack
    // Clobbers: r12
    asm!(
        "
        push {{r0, r4}} // r0 be used as scratch register for throw-away loads
        mov  r12, #-8   // Stack offset; initial value accounts for saved registers

    1:  // Repeatedly touch the guard page to trigger faults and allocate more stack
        sub  r12, r12, 4096  // Page size in bytes
        ldr  r0,  [sp, r12]
        subs r4,  r4,  1024  // Page size in 4-byte units
        bgt  1b

        pop  {{r0, r4}}
        lsl  r4,  2     // Convert 4-byte units to single bytes, as expected by caller
        blx  lr
        "
    )
}

// These are Microsoft C Runtime Library functions that LLVM expects to be available when
// it is making PE files for Arm.
cfg_if::cfg_if! {
    if #[cfg(all(target_os = "uefi", target_arch = "arm"))] {
        /// Convert 64-bit unsigned int to double-precision float
        #[no_mangle]
        pub unsafe extern "C" fn __u64tod(i: u64) -> f64 {
            __floatundidf(i)
        }

        /// Convert 64-bit unsigned int to single-precision float
        #[no_mangle]
        pub unsafe extern "C" fn __u64tos(i: u64) -> f32 {
            __floatundisf(i)
        }

        /// Division with remainder for unsigned 32-bit integers
        #[no_mangle]
        pub unsafe extern "C" fn __rt_udiv(d: u32, n: u32) -> rt_div_result<u32> {
            let mut result: rt_div_result<u32> = rt_div_result::default();
            result.q = __udivmodsi4(n, d, Some(&mut result.rem));
            result
        }

        /// Division with remainder for unsigned 64-bit integers
        #[no_mangle]
        pub unsafe extern "C" fn __rt_udiv64(d: u64, n: u64) -> rt_div_result<u64> {
            let mut result: rt_div_result<u64> = rt_div_result::default();
            result.q = __udivmoddi4(n, d, Some(&mut result.rem));
            result
        }

        /// Division with remainder for signed 32-bit integers
        #[no_mangle]
        pub unsafe extern "C" fn __rt_sdiv(d: i32, n: i32) -> rt_div_result<i32> {
            let mut result: rt_div_result<i32> = rt_div_result::default();
            result.q = __divmodsi4(n, d, &mut result.rem);
            result
        }

        /// Division with remainder for unsigned 64-bit integers
        #[no_mangle]
        pub unsafe extern "C" fn __rt_sdiv64(d: i64, n: i64) -> rt_div_result<i64> {
            let mut result: rt_div_result<i64> = rt_div_result::default();
            result.q = __divmoddi4(n, d, &mut result.rem);
            result
        }

        /// Quotient and remainder
        #[repr(C)]
        #[derive(Debug, Default)]
        pub struct rt_div_result<T> {
            q: T,
            rem: T,
        }

        extern "C" {
            // Functions from compiler-builtins that corespond to the MS CRT __u64to*
            // functions above.
            fn __floatundidf(i: u64) -> f64;
            fn __floatundisf(i: u64) -> f32;

            // Functions from compiler-builtins that correspond to the MS CRT __rt_*div*
            // functions above. Note that they have swapped arguments compared to CRT.
            fn __udivmodsi4(n: u32, d: u32, rem: Option<&mut u32>) -> u32;
            fn __udivmoddi4(n: u64, d: u64, rem: Option<&mut u64>) -> u64;
            fn __divmodsi4(n: i32, d: i32, rem: &mut i32) -> i32;
            fn __divmoddi4(n: i64, d: i64, rem: &mut i64) -> i64;

        }
    }
}
