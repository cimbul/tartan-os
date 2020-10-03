//! Support for protected mode operation.
//!
//! This includes the minimal support for segmented memory and hardware task management
//! that is required to operate in protected mode with a flat memory model.

use crate::x86_common::protection::{Selector, IOPermissionBitmap};
use static_assertions::const_assert_eq;

#[cfg(doc)]
use crate::x86_common::FlagRegister;
#[cfg(doc)]
use crate::x86_common::paging::ControlRegister3;
#[cfg(doc)]
use crate::x86_common::protection::{LocalDescriptorTableRegister, GateDescriptorFlags};


/// A task state segment (TSS) with fixed redirect and I/O permission maps.
///
/// From the processor's perspective, the layout of the TSS is flexible after the end of
/// the [`TaskStateSegmentHeader`]. The permission map may be at any offset or missing
/// from the segment completely. For that reason, this struct is only suitable for use in
/// setting up a TSS that the caller owns. For reading a TSS created by another system
/// (e.g., the bootloader), use the [`TaskStateSegmentHeader`] struct on its own.
#[repr(C)]
#[derive(Debug, PartialEq, Eq)]
pub struct BasicTaskStateSegment {
    /// Saved task data and pointer to bitmaps
    pub header: TaskStateSegmentHeader,
    /// I/O permission map and interrupt redirect map
    pub bitmaps: TaskStateSegmentBitmaps<[u8; IOPermissionBitmap::MAX_SIZE]>,
}


/// Saved task state that makes up the most significant part of a task state segment
/// (TSS).
///
/// Some of the fields of this header are automatically saved by the processor when
/// switching tasks. Refer to each field's documentation.
///
/// This header can be followed by OS-specific data and/or the
/// [`TaskStateSegmentBitmaps`]. The location of the latter is determined by
/// [`io_permission_map_offset`](Self::io_permission_map_offset).
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TaskStateSegmentHeader {
    /// Segment selector saved by the processor referencing the task that was running
    /// before this task was activated.
    ///
    /// If [`FlagRegister::nested_task`] is set, then this task will be resumed by the
    /// `IRET` instruction.
    pub previous_task: u16,
    _reserved_a: u16,
    /// Stack pointer and segment for privilege levels 0–2.
    ///
    /// Not modified by the processor on a task switch.
    pub privileged_stack: [PrivilegedStack; 3],
    /// Page table pointer from [`ControlRegister3`].
    ///
    /// Not modified by the processor on a task switch.
    pub control_register_3: u32,
    /// `EIP` value saved by the processor.
    pub instruction_pointer: u32,
    /// `EFLAGS` value saved by the processor.
    pub flags: u32,
    /// `EAX` value saved by the processor.
    pub general_a: u32,
    /// `ECX` value saved by the processor.
    pub general_c: u32,
    /// `EDX` value saved by the processor.
    pub general_d: u32,
    /// `EBX` value saved by the processor.
    pub general_b: u32,
    /// `SP` value saved by the processor for privilege level 3.
    pub stack_pointer: u32,
    /// `EBP` value saved by the processor.
    pub base_pointer: u32,
    /// `ESI` value saved by the processor.
    pub source_index: u32,
    /// `EDI` value saved by the processor.
    pub destination_index: u32,
    /// `ES` value saved by the processor.
    pub general_segment_e: Selector,
    _reserved_b: u16,
    /// `CS` value saved by the processor.
    pub code_segment: Selector,
    _reserved_c: u16,
    /// `SS` value saved by the processor for privilege level 3.
    pub stack_segment: Selector,
    _reserved_d: u16,
    /// `DS` value saved by the processor.
    pub data_segment: Selector,
    _reserved_e: u16,
    /// `FS` value saved by the processor.
    pub general_segment_f: Selector,
    _reserved_f: u16,
    /// `GS` value saved by the processor.
    pub general_segment_g: Selector,
    _reserved_g: u16,
    /// [`LocalDescriptorTableRegister`] value to load when executing this task.
    ///
    /// Not modified by the processor on a task switch.
    pub local_descriptor_table: Selector,
    _reserved_h: u16,
    /// Indicates that a debug exception should be raised when this task is activated.
    ///
    /// Not modified by the processor on a task switch.
    pub debug_trap: bool,
    /// Offset from the start of this structure to start of the [`IOPermissionBitmap`],
    /// and to the end of the [`interrupt_redirect`
    /// bitmap](TaskStateSegmentBitmaps::interrupt_redirect).
    ///
    /// The I/O permission map ends at the limit of the containing segment, and must be at
    /// least two bytes. If this offset is equal to or greater than the limit, then the
    /// permission map is empty and all ports are assumed to be zero.
    ///
    /// The interrupt redirect bitmap begins at this offset minus 32.
    ///
    /// Not modified by the processor on a task switch.
    pub io_permission_map_offset: u16,
    /// Pointer to the shadow stack.
    ///
    /// Not modified by the processor on a task switch.
    pub shadow_stack_pointer: u32,
}

const_assert_eq!(108, core::mem::size_of::<TaskStateSegmentHeader>());


/// Stack pointer and segment for a given privilege level
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PrivilegedStack {
    /// Stack pointer (`SP`) value for this privilege level.
    pub pointer: u32,
    /// Stack segment (`SS`) value for this privilege level.
    pub segment: Selector,
}

const_assert_eq!(8, core::mem::size_of::<PrivilegedStack>());



/// I/O permission map and interrupt redirect map, which are always located together in
/// a task state segment (TSS).
#[repr(C)]
#[derive(Debug, PartialEq, Eq)]
pub struct TaskStateSegmentBitmaps<T>
where
    T: AsRef<[u8]> + AsMut<[u8]> + Eq + ?Sized {
    /// Indicates which handler to use for software-triggered interrupts in virtual real
    /// mode.
    ///
    /// If bit `N` (bit `N mod 8` of byte `floor(N / 8)`) is set, it indicates that an
    /// `INT N` in virtual real mode should be handled using the protected-mode mechanism
    /// rather than the process's own interrupt table.
    pub interrupt_redirect: [u8; 32],

    /// The [`IOPermissionBitmap`] for this task.
    pub io_permission: IOPermissionBitmap<T>,
}
