#![no_std]
#![feature(alloc_error_handler)]
#![feature(asm)]
#![feature(global_asm)]
#![feature(lang_items)]
#![feature(link_args)]
#![feature(naked_functions)]
#![feature(panic_info_message)]
#![feature(rustc_private)]
#![feature(start)]

extern crate alloc;

use alloc::string::String;
use core::fmt::Write;
use core::mem::MaybeUninit;
use tartan_serial::{LineMode, UARTWriteAdapter, UART};


mod allocator;
mod cpu;
mod intrinsics;
mod pci;


#[cfg(target_os = "tartan")]
global_asm!(
    "
    .section .bss
    .balign 16
stack_bottom:
    .skip 0x4000
    .global stack_top
stack_top:
    "
);


#[used]
static mut X: usize = 0;

// Dummy implementation when building for host
#[start]
#[cfg(not(target_os = "tartan"))]
fn _start(_: isize, _: *const *const u8) -> isize {
    kernel_main()
}

#[start]
#[naked]
#[no_mangle]
#[cfg(target_os = "tartan")]
fn _start(_: isize, _: *const *const u8) -> isize {
    unsafe {
        #[cfg(target_arch = "x86")]
        asm!(
            "
            mov esp, offset stack_top  // Set up initial stack
            call {}                    // Call real main function
        2:  hlt                        // Halt if main function ever returns
            jmp 2b
            ",
            sym kernel_main,
            options(noreturn),
        );

        #[cfg(target_arch = "x86_64")]
        asm!(
            "
            mov rsp, offset stack_top  // Set up initial stack
            call {}                    // Call real main function
        2:  hlt                        // Halt if main function ever returns
            jmp 2b
            ",
            sym kernel_main,
            options(noreturn),
        );

        #[cfg(target_arch = "arm")]
        asm!(
            "
            ldr sp, =stack_top  // Set up initial stack
            blx {}              // Call real main function
        1:  b 1b                // Spin if main function ever returns
            ",
            sym kernel_main,
            options(noreturn),
        );

        #[cfg(target_arch = "aarch64")]
        asm!(
            "
            ldr x0, =stack_top  // Set up initial stack
            mov sp, x0
            bl {}               // Call real main function
        1:  wfe                 // Halt if main function ever returns
            b 1b
            ",
            sym kernel_main,
            options(noreturn),
        );
    }
}

fn kernel_main() -> ! {
    let mut serial = find_uart();
    serial.reset();
    serial.set_line_mode(LineMode::default());
    let mut out = UARTWriteAdapter(serial);

    writeln!(out, "Hello, world!").unwrap();

    unsafe {
        ALLOCATOR.init(allocator::BlockList::from_block(&mut HEAP));
    }

    let mut heap_message = String::from("This came from...");
    heap_message.push_str("the heap!");
    writeln!(out, "{}", heap_message).unwrap();

    cpu::print_state(&mut out).unwrap();
    pci::print_devices(&mut out).unwrap();

    writeln!(out, "Done doing anything useful.").unwrap();

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
    use tartan_serial::model_pl011::{RegisterBlock, UARTPL011};

    // FIXME: Hardcoded values copied from QEMU's "virt" board device tree
    UARTPL011 {
        register_block: unsafe { &mut *(0x0900_0000 as *mut RegisterBlock) },
        base_clock_rate: 24_000_000,
    }
}


#[cfg_attr(not(test), global_allocator)]
static mut ALLOCATOR: allocator::Allocator = allocator::Allocator::uninitialized();

static mut HEAP: [MaybeUninit<usize>; 0x8000] = [MaybeUninit::uninit(); 0x8000];

#[cfg(not(test))]
#[alloc_error_handler]
fn alloc_error(layout: core::alloc::Layout) -> ! {
    panic!("Allocating {} bytes failed", layout.size());
}

#[cfg(not(test))]
#[panic_handler]
fn panic_handler(info: &core::panic::PanicInfo) -> ! {
    // Not much we can do with Err results in a panic handler
    #![allow(unused_must_use)]

    let mut serial = find_uart();
    serial.reset();
    serial.set_line_mode(LineMode::default());
    let mut out = UARTWriteAdapter(serial);

    writeln!(out, "!!! Panic !!!");
    match info.location() {
        Some(location) => writeln!(out, "Location: {}", location),
        None => writeln!(out, "No location information"),
    };
    match info.message() {
        Some(arguments) => core::fmt::write(&mut out, *arguments),
        None => writeln!(out, "No additional message"),
    };

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
