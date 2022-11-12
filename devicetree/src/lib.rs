//! Support for the [Devicetree](https://www.devicetree.org/) specification.

#![no_std]
#![feature(const_size_of_val)]
#![warn(missing_docs)]
#![warn(clippy::pedantic)]
#![allow(clippy::must_use_candidate)]
#![allow(clippy::upper_case_acronyms)]

use core::fmt;

pub mod blob;


/// Wrapper for a raw byte array used as the value of a Devicetree property. Used for
/// intelligent formatting of unknown properties.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Value<'a> {
    /// Property value as raw bytes
    pub data: &'a [u8],
}

impl<'a> Value<'a> {
    /// Returns true if this looks like a null-terminated ASCII string or an array of
    /// them.
    fn is_string_like(&self) -> bool {
        let data = self.data;
        if data.is_empty() {
            return false;
        }
        // Must have at least a final null terminator.
        if data.last() != Some(&0) {
            return false;
        }
        // Don't count values that are entirely null.
        if data.iter().all(|&c| c == 0) {
            return false;
        }
        data.iter().all(|&c| c == 0 || c.is_ascii_graphic() || c.is_ascii_whitespace())
    }
}

impl<'a> fmt::Debug for Value<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // Write 32-bit hex "cells"
        f.write_str("<")?;
        for (i, &b) in self.data.iter().enumerate() {
            let space = if i > 0 && i % 4 == 0 { " " } else { "" };
            write!(f, "{space}{b:02x}")?;
        }
        f.write_str(">")?;

        // If this looks like a string (or array of strings), print it
        if self.is_string_like() {
            let init = &self.data[..self.data.len() - 1];
            for segment in init.split(|&c| c == 0) {
                let string_segment = core::str::from_utf8(segment).unwrap();
                write!(f, " \"{string_segment}\"")?;
            }
        }

        Ok(())
    }
}


/// Indicates an area of memory that the kernel does not have full control over: e.g.,
/// ROMs, firmware.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct MemoryReservation {
    /// Start address of the reserved memory
    pub address: u64,
    /// Size of the reserved memory in bytes
    pub size: u64,
}
