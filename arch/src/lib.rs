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
arch_module!(x86_common, any(target_arch = "x86", target_arch = "x86_64"));
