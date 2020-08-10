#![no_std]
#![feature(ptr_offset_from)]
#![deny(clippy::pedantic)]
#![allow(clippy::must_use_candidate)]

extern crate alloc;

pub mod efi;
pub mod acpi;
