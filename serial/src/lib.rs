//! Support for Universal Asynchronous Receiver/Transmitter (UART) devices

#![no_std]
#![feature(doc_cfg)]
#![warn(missing_docs)]
#![warn(clippy::pedantic)]
#![allow(clippy::must_use_candidate)]
#![allow(clippy::upper_case_acronyms)]


pub mod model_pl011;

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


/// Newtype wrapper that allows a UART implementation to be used by [`core::fmt::write`].
pub struct UARTWriteAdapter<T: UART>(pub T);

impl<T: UART> core::fmt::Write for UARTWriteAdapter<T> {
    fn write_str(&mut self, s: &str) -> core::fmt::Result {
        // Transform "\n" into "\r\n" while writing
        let bytes = s.as_bytes();
        let mut first = true;
        for chunk in bytes.split(|&c| c == b'\n') {
            if first {
                first = false;
            } else {
                self.0.write(b"\r\n");
            }
            self.0.write(chunk);
        }
        Ok(())
    }
}


/// Speed and other line protocol settings.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LineMode {
    /// Bits per second (including non-data bits) of line protocol.
    pub baud: u32,
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
            baud: 9600,
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

impl Parity {
    fn from_flags(enabled: bool, even: bool, sticky: bool) -> Self {
        if enabled {
            match (sticky, even) {
                (false, false) => Self::Odd,
                (false, true) => Self::Even,
                (true, false) => Self::High,
                (true, true) => Self::Low,
            }
        } else {
            Self::None
        }
    }

    fn enabled_flag(self) -> bool {
        self != Self::None
    }

    fn even_flag(self) -> bool {
        self == Self::Even || self == Self::Low
    }

    fn sticky_flag(self) -> bool {
        self == Self::High || self == Self::Low
    }
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


/// Common two-bit register encoding for setting the data word length.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum WordLength {
    Five,
    Six,
    Seven,
    Eight,
}

impl WordLength {
    pub fn bits(self) -> u8 {
        match self {
            Self::Five => 5,
            Self::Six => 6,
            Self::Seven => 7,
            Self::Eight => 8,
        }
    }

    pub fn from_bits(bits: u8) -> Option<WordLength> {
        match bits {
            5 => Some(Self::Five),
            6 => Some(Self::Six),
            7 => Some(Self::Seven),
            8 => Some(Self::Eight),
            _ => None,
        }
    }
}

impl From<u8> for WordLength {
    fn from(value: u8) -> Self {
        match value & 0b11 {
            0 => Self::Five,
            1 => Self::Six,
            2 => Self::Seven,
            3 => Self::Eight,
            // For two bit values, the cases above are exhaustive
            _ => unreachable!(),
        }
    }
}

impl From<WordLength> for u8 {
    fn from(value: WordLength) -> Self {
        value as u8
    }
}



fn baud_from_divisor(max_baud: u32, divisor: u32) -> u32 {
    if divisor == 0 {
        0
    } else {
        max_baud / divisor
    }
}

fn divisor_from_baud(max_baud: u32, baud: u32) -> u32 {
    if baud == 0 || baud >= max_baud {
        1
    } else {
        max_baud / baud
    }
}
