#![no_std]
#![cfg_attr(not(test), no_main)]
#![cfg_attr(not(test), feature(lang_items))]
#![cfg_attr(not(test), feature(link_args))]
#![feature(alloc_error_handler)]
#![feature(asm)]
#![feature(naked_functions)]
#![feature(panic_info_message)]
#![feature(rustc_private)]
#![feature(test)]
#![warn(clippy::pedantic)]
#![allow(clippy::must_use_candidate)]

extern crate alloc;

mod intrinsics;

#[cfg(not(test))]
use core::alloc::Layout;
#[cfg(not(test))]
use core::panic::PanicInfo;
#[cfg(not(test))]
use tartan_uefi::allocator::BootAllocator;

use alloc::string::String;
use core::fmt::Write;
use core::mem;
use log::info;
use tartan_elf as elf;
use tartan_uefi::global::SYSTEM_TABLE;
use tartan_uefi::io::{encode_c_utf16, Logger, OutputStream};
use tartan_uefi::proto::{File, FileAttributes, FileMode, LoadedImage, SimpleFileSystem};
use tartan_uefi::writeln_result;
use tartan_uefi::{
    AllocateType, BootServices, Handle, MemoryType, Result, Status, SystemTable, Table,
    PAGE_SIZE,
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
        Err(status) | Ok(status) => {
            // It's not normal to return even if it's success, so panic either way
            panic!("Main returned {:?}", status)
        }
    }
}

fn efi_main_result(image_handle: Handle, system_table: &mut SystemTable) -> Result {
    let mut out = OutputStream::new(&*system_table.console_out.unwrap());
    writeln_result!(out, "Hello, world!\nWhat's up?")?;

    info!("Logging initialized");

    writeln_result!(out, "Verifying system tables...")?;
    system_table.verify();
    system_table.runtime_services.verify();
    system_table.boot_services.unwrap().verify();
    writeln_result!(out, "Verified!")?;

    let boot_services = system_table.boot_services.unwrap();
    let loaded_image =
        boot_services.get_protocol::<LoadedImage>(image_handle, image_handle)?;
    let image_base = loaded_image.image_base;
    writeln_result!(out, "Image base: {:p}", image_base)?;

    // fakepoint();

    writeln_result!(out, "Testing compiler intrinsics...")?;
    intrinsics::test();
    writeln_result!(out, "Success!")?;

    writeln_result!(out, "Attempting heap allocation")?;
    writeln_result!(out, "Testing... {}", String::from("Success!"))?;

    writeln_result!(out, "Fetching memory map")?;
    let memory_map = boot_services.get_memory_map();
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
            "  #{:2}: {:8x} => {:x} + {:4x} pages, attr = {:16x}, {:?}",
            i,
            descriptor.physical_start,
            descriptor.virtual_start,
            descriptor.page_count,
            u64::from(descriptor.attributes),
            descriptor.memory_type
        )?;
    }

    print_device_tree(&mut out)?;

    load_kernel(
        &mut out,
        image_handle,
        loaded_image,
        system_table,
        "EFI\\BOOT\\TARTAN.ELF",
    )?;

    Ok(Status::Success)
}

/// A fake breakpoint. Stay in an endless loop until we can attach a debugger and manually
/// return.
#[allow(dead_code)]
fn fakepoint() {
    #[allow(clippy::empty_loop)]
    loop {}
}

#[cfg(not(any(target_arch = "arm", target_arch = "aarch64")))]
fn print_device_tree(_: &mut OutputStream) -> Result {
    #![allow(clippy::unnecessary_wraps)]
    // TODO
    Ok(Status::Success)
}

#[cfg(any(target_arch = "arm", target_arch = "aarch64"))]
fn print_device_tree(out: &mut OutputStream) -> Result {
    use tartan_devicetree::blob::{StructureData, Tree};

    writeln_result!(out, "Devicetree:")?;

    let devicetree = unsafe { Tree::from_ptr::<()>(0x4000_0000 as *const u8) }
        .expect("Error parsing Devicetree");
    let mut indent = 0_usize;
    for structure_data_result in devicetree.structure_iter::<()>() {
        let structure_data = structure_data_result.expect("Error parsing Devicetree");
        match structure_data {
            StructureData::BeginNode(name) => {
                let display_name = if name.is_empty() { "/" } else { name };
                writeln_result!(out, "{0:1$}{2} {{", "", 2 * indent, display_name,)?;
                indent += 1;
            }
            StructureData::EndNode => {
                indent -= 1;
                writeln_result!(out, "{0:1$}}}", "", 2 * indent,)?;
            }
            StructureData::Property { name, value } => {
                writeln_result!(out, "{0:1$}{2} = {3:?}", "", 2 * indent, name, value,)?;
            }
        }
    }

    Ok(Status::Success)
}


fn load_kernel(
    out: &mut OutputStream,
    image_handle: Handle,
    loaded_image: &LoadedImage,
    system_table: &mut SystemTable,
    path: &str,
) -> Result {
    writeln_result!(out, "Looking for kernel image at {}...", path)?;
    let boot_services = system_table.boot_services.unwrap();
    let file_system = boot_services
        .get_protocol::<SimpleFileSystem>(loaded_image.device_handle, image_handle)?;
    let root_dir = file_system.open_volume()?;
    let kernel_file = root_dir.open(
        &encode_c_utf16(path),
        FileMode::Read,
        FileAttributes::default(),
    )?;

    writeln_result!(out, "Verifying kernel image...")?;
    let header = unsafe { read_struct::<elf::HeaderNative>(kernel_file)? };
    header.verify_native();

    writeln_result!(out, "Loading kernel image...")?;
    for i in 0..usize::from(header.program_header_count) {
        let position =
            header.program_header_offset + usize::from(header.program_header_size) * i;
        kernel_file.set_position(position as u64)?;

        let program_header =
            unsafe { read_struct::<elf::ProgramHeaderNative>(kernel_file)? };
        if program_header.segment_type == elf::ProgramSegmentType::Loadable {
            writeln_result!(
                out,
                "  Loading segment {} to address {:#x} (size {:#x})",
                i,
                program_header.physical_addr,
                program_header.mem_size,
            )?;

            load_elf_segment(boot_services, kernel_file, &program_header)?;
        } else {
            writeln_result!(
                out,
                "  Skipping segment {} with type {:?}",
                i,
                program_header.segment_type,
            )?;
        }
    }

    writeln_result!(out, "Done loading kernel image.")?;

    writeln_result!(
        out,
        "Exiting boot services and transferring control to kernel entry point {:#x}",
        header.entry_point,
    )?;

    let _memory_map = unsafe { system_table.exit_boot_services(image_handle) };
    unsafe {
        tartan_arch::jump(header.entry_point);
    }
}

unsafe fn read_struct<T: Default>(file: &File) -> core::result::Result<T, Status> {
    let mut result = T::default();
    let size = mem::size_of::<T>();
    let read_count = file
        .read(core::slice::from_raw_parts_mut(&mut result as *mut T as *mut u8, size))?;
    if read_count == size {
        Ok(result)
    } else {
        Err(Status::EndOfFile)
    }
}

fn load_elf_segment(
    boot_services: &BootServices,
    file: &File,
    program_header: &elf::ProgramHeaderNative,
) -> Result {
    let start_addr = as_usize(program_header.physical_addr);
    let end_addr = start_addr + as_usize(program_header.mem_size);

    // Reserve the pages required for this segment
    let start_page = start_addr / PAGE_SIZE;
    let end_page = if end_addr == 0 { 0 } else { 1 + (end_addr - 1) / PAGE_SIZE };
    boot_services.allocate_pages(
        AllocateType::ExactAddress,
        MemoryType::LoaderData,
        end_page - start_page,
        Some((start_page * PAGE_SIZE) as u64),
    )?;

    // Load the part of the section contained in the file
    if program_header.file_size > 0 {
        #[allow(clippy::useless_conversion)] // Not useless on 32-bit platforms
        file.set_position(u64::from(program_header.file_offset))?;
        let read_count = unsafe {
            file.read(core::slice::from_raw_parts_mut(
                start_addr as *mut u8,
                as_usize(program_header.file_size),
            ))?
        };
        assert_eq!(
            read_count,
            as_usize(program_header.file_size),
            "Could not read full program segment from file",
        );
    }

    // Clear the BSS portion if present
    if program_header.mem_size > program_header.file_size {
        let zero_start_addr = start_addr + as_usize(program_header.file_size);
        unsafe {
            // TODO: Import memset? I assume this will optimize to it anyway.
            for addr in zero_start_addr..end_addr {
                *(addr as *mut u8) = 0;
            }
        }
    }

    Ok(Status::Success)
}

// The ELF format uses u32 to represent addresses for 32-bit systems. Define this function
// to silence the spurious clippy warning (without scattering the code with #![allow]).
#[cfg(target_pointer_width = "32")]
fn as_usize(n: u32) -> usize {
    #![allow(clippy::cast_possible_truncation)]
    n as usize
}

// The ELF format uses u64 to represent addresses for 64-bit systems. Define this function
// to silence the spurious clippy warning (without scattering the code with #![allow]).
#[cfg(target_pointer_width = "64")]
fn as_usize(n: u64) -> usize {
    #![allow(clippy::cast_possible_truncation)]
    n as usize
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

    #[allow(clippy::empty_loop)]
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
    #[allow(clippy::empty_loop)]
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
