#![no_std]
#![feature(llvm_asm)]

use cfg_if::cfg_if;

cfg_if! {
    if #[cfg(target_arch = "x86_64")] {
        mod x86_64;
        pub use x86_64::*;
    } else if #[cfg(target_arch = "x86")] {
        mod x86;
        pub use x86::*;
    }
}

#[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
mod x86_common;
