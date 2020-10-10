//! Architecture-specific bindings for Tartan OS

#![no_std]
#![feature(asm)]
#![feature(doc_cfg)]
#![warn(missing_docs)]
#![warn(clippy::pedantic)]
#![allow(clippy::must_use_candidate)]

macro_rules! arch_module {
    [$mod_name:ident, $( $requirements:tt )+ $(,)?] => {
        #[cfg(any(all( $($requirements)+ ), doc))]
        #[doc(cfg(all( $($requirements)+ )))]
        pub mod $mod_name;
    };
}

arch_module!(x86, target_arch = "x86");
arch_module!(x86_64, target_arch = "x86_64");

// We can't use #[macro_use] from within a macro
#[cfg(any(target_arch = "x86", target_arch = "x86_64", doc))]
#[doc(cfg(any(target_arch = "x86", target_arch = "x86_64")))]
#[macro_use]
pub mod x86_common;


/// Unconditionally transfer control to the instruction at the given address
///
/// # Safety
/// Make sure you know where you're going. You're not coming back.
#[cfg(any(target_arch = "x86", target_arch = "x86_64", doc))]
pub unsafe fn jump(address: usize) -> ! {
    asm!("jmp {}", in(reg) address);
    unreachable!();
}

/// Unconditionally transfer control to the instruction at the given address
///
/// # Safety
/// Make sure you know where you're going. You're not coming back.
#[cfg(target_arch = "arm")]
pub unsafe fn jump(address: usize) -> ! {
    asm!("bx {}", in(reg) address);
    unreachable!();
}

/// Unconditionally transfer control to the instruction at the given address
///
/// # Safety
/// Make sure you know where you're going. You're not coming back.
#[cfg(target_arch = "aarch64")]
pub unsafe fn jump(address: usize) -> ! {
    asm!("br {}", in(reg) address);
    unreachable!();
}
