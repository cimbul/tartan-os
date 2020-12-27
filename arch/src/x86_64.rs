//! Architecture-specific primitives for 64-bit x86 processors.

use crate::{model_specific_register_access, simple_register_access};
use tartan_bitfield::bitfield;

pub mod protection;


bitfield! {
    /// `CR8`: System control register for controlling interrupts based on priority.
    ///
    /// Getters and setters for this structure only access a value in memory, not the
    /// register itself. Use the [`get`](Self::get) and [`set`](Self::set) methods to
    /// work with the actual register.
    pub struct ControlRegister8(usize) {
        /// `CR8.TPL`: Threshold for blocking low-priority interrupts. Lower values are
        /// higher priority.
        [0..4] pub task_priority_level: u8,
    }
}

simple_register_access!(ControlRegister8, "cr8");


bitfield! {
    /// `IA32_EFER`: Model-specific register that controls features relating to 64-bit
    /// operation.
    pub struct ExtendedFeatureEnableRegister(u64) {
        /// `IA32_EFER.SCE`: Support `SYSCALL`/`SYSRET` in 64-bit mode.
        [0] pub syscall,
        /// `IA32_EFER.LME`: Support 64-bit mode.
        [8] pub long_mode_enabled,
        /// `IA32_EFER.LMA`: Indicates 64-bit mode is active. Read-only.
        ///
        /// This is logically the equivalent to `[Self::long_mode_enabled] &
        /// [ControlRegister0::paging]`.
        [10] pub long_mode_active,
        /// `IA32-EFER.NXE`: Support no-execute (NX) bit in page tables.
        [11] pub no_execute,
    }
}

model_specific_register_access!(ExtendedFeatureEnableRegister, 0xc000_0080_u32);
