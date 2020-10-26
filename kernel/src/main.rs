#![no_std]
#![feature(asm)]
#![feature(global_asm)]
#![feature(lang_items)]
#![feature(link_args)]
#![feature(naked_functions)]
#![feature(rustc_private)]
#![feature(start)]

use tartan_serial::{LineMode, UART};


mod intrinsics;


global_asm!(
    "
    .section .bss
    .balign 16
stack_bottom:
    .skip 0x1000
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
