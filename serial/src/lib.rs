//! Support for Universal Asynchronous Receiver/Transmitter (UART) devices

#![no_std]
#![feature(doc_cfg)]
#![warn(missing_docs)]
#![warn(clippy::pedantic)]
#![allow(clippy::must_use_candidate)]


#[cfg(any(target_arch = "x86", target_arch = "x86_64", doc))]
#[doc(cfg(any(target_arch = "x86", target_arch = "x86_64")))]
pub mod model_16550;


/// High-level interface for UART drivers.
pub trait UART {
    /// Stop transmission, clear buffers, and set the device to a sane state.
    fn reset(&mut self);
    /// Get the device's active line protocol mode.
    fn line_mode(&mut self) -> LineMode;
    /// Update the device's line protocol mode.
    fn set_line_mode(&mut self, mode: LineMode);
    /// Send the given data through the UART. Blocks until all data is set.
    fn write(&mut self, data: &[u8]);
}


/// Speed and other line protocol settings.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LineMode {
    /// Baud rate of line protocol.
    pub bits_per_second: u32,
    /// Number of data bits per word in line protocol.
    pub data_bits: u8,
    /// Parity setting for line protocol.
    pub parity: Parity,
    /// Use 1.5 or 2 stop bits (depending on baud rate) if set. If clear, use 1 stop bit.
    pub extended_stop: bool,
}

impl Default for LineMode {
    fn default() -> LineMode {
        LineMode {
            bits_per_second: 9600,
            data_bits: 8,
            parity: Parity::default(),
            extended_stop: false,
        }
    }
}


/// Parity setting for line protocol.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Parity {
    /// No parity bit.
    None,
    /// Set parity so that there are an odd number of high bits in the word.
    Odd,
    /// Set parity so that there are an even number of high bits in the word.
    Even,
    /// Set parity bit high unconditionally (sticky).
    High,
    /// Set parity bit low unconditionally (sticky).
    Low,
}

impl Default for Parity {
    fn default() -> Self {
        Self::None
    }
}


/// Dummy UART driver that drops outgoing data and never receives anything.
pub struct NullUART;

impl UART for NullUART {
    fn reset(&mut self) {}
    fn set_line_mode(&mut self, _: LineMode) {}
    fn write(&mut self, _: &[u8]) {}

    fn line_mode(&mut self) -> LineMode {
        LineMode::default()
    }
}
