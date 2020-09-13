//! Architecture-specific bindings for Tartan OS

#![no_std]
#![feature(asm)]
#![warn(missing_docs)]
#![warn(clippy::pedantic)]
#![allow(clippy::must_use_candidate)]

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
