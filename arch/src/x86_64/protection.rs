//! Support for protected mode operation.
//!
//! This includes the minimal support for segmented memory and hardware task management
//! that is required to operate in protected mode with a flat memory model.

use core::mem::size_of;

#[cfg(doc)]
use crate::x86_common::protection::{GateDescriptorFlags, IOPermissionBitmap};


/// Stack and I/O permission map pointers that make up the most significant part of a task
/// state segment (TSS).
///
/// Since 64-bit mode does not support task switching, the name is a historical artifact,
/// and this does not store any real task state.
#[repr(C, packed(16))]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TaskStateSegmentHeader {
    _reserved_a: u32,

    /// Stack pointers to use when switching to privilege levels 0–2.
    pub privileged_stack: [u64; 3],

    _reserved_b: u64,

    /// Stack pointers available for use when handling interrupts. The specific entry
    /// used is determined by the
    /// [`interrupt_stack_index`](GateDescriptorFlags::interrupt_stack_index) field of
    /// the relevant interrupt gate descriptor.
    pub interrupt_stack: [u64; 7],

    _reserved_c: u64,
    _reserved_d: u16,

    /// Offset from the start of this structure to start of the [`IOPermissionBitmap`].
    ///
    /// The I/O permission map ends at the limit of the containing segment, and must be
    /// at least two bytes. If this offset is equal to or greater than the limit,
    /// then the permission map is empty and all ports are assumed to be zero.
    ///
    /// Not modified by the processor on a task switch.
    pub io_permission_map_offset: u16,
}

impl TaskStateSegmentHeader {
    /// Create a header with zero-initialized stack pointers and an
    /// [`io_permission_map_offset`](Self::io_permission_map_offset) that points directly
    /// after the header.
    pub const fn new() -> Self {
        #[allow(clippy::cast_possible_truncation)]
        Self {
            privileged_stack: [0; 3],
            interrupt_stack: [0; 7],
            io_permission_map_offset: size_of::<Self>() as u16,

            _reserved_a: 0,
            _reserved_b: 0,
            _reserved_c: 0,
            _reserved_d: 0,
        }
    }
}

impl Default for TaskStateSegmentHeader {
    fn default() -> Self {
        Self::new()
    }
}
