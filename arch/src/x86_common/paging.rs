//! Support for virtual memory paging.

use crate::simple_register_access;
use tartan_bitfield::{bitfield, bitfield_accessors, Bitfield};

#[cfg(doc)]
use super::features::BasicFeatures;
#[cfg(doc)]
use super::ControlRegister4;
#[cfg(doc)]
use crate::x86_64::ExtendedFeatureEnableRegister;


bitfield! {
    /// `CR2`: Contains the address that triggered a page fault.
    pub struct ControlRegister2(usize) {}
}

simple_register_access!(ControlRegister2, "cr2");


bitfield! {
    /// `CR3`: System control register that contains the top-level page table address
    /// and two associated caching flags.
    ///
    /// Getters and setters for this structure only access a value in memory, not the
    /// register itself. Use the [`get`](Self::get) and [`set`](Self::set) methods to work
    /// with the actual register.
    pub struct ControlRegister3(usize) {
        /// `CR3.PWT`: Enables write-through caching for the top-level page table.
        ///
        /// Does not apply when [`ControlRegister4::process_context_ids`] or
        /// [`ControlRegister4::physical_address_extension`] is enabled.
        [3] pub write_through,

        /// `CR3.PCD`: Disables caching for the top-level page table.
        ///
        /// Does not apply when [`ControlRegister4::process_context_ids`] or
        /// [`ControlRegister4::physical_address_extension`] is enabled.
        [4] pub cache_disabled,

        /// `CR4.PCIDE`: The process-context identifier (PCID) associated with this
        /// series of page tables.
        ///
        /// Requires 64-bit mode and [`ControlRegister4::process_context_ids`].
        #[cfg(any(target_arch = "x86_64", doc))]
        #[doc(cfg(target_arch = "x86_64"))]
        [0..11] pub process_context_id: u16,
    }
}

simple_register_access!(ControlRegister3, "cr3");

impl ControlRegister3 {
    const ADDRESS_MASK: usize = !0xfff;

    /// Get the address of the top-level page table.
    pub fn address(self) -> usize {
        self.0 & Self::ADDRESS_MASK
    }

    /// Set the address of the top-level page table.
    ///
    /// # Panics
    /// Panics if the new address is not 4K-aligned.
    pub fn set_address(&mut self, value: usize) {
        assert!(
            value & Self::ADDRESS_MASK == 0,
            "Invalid page table address {:#x}. Must be 4K aligned.",
            value
        );
        self.0 &= !Self::ADDRESS_MASK;
        self.0 |= value;
    }
}


/// An entry in a page table at any level.
pub trait GenericPageTableEntry: Bitfield<usize> {
    bitfield_accessors! {
        /// `P`: Indicates that this entry is mapped. Otherwise the whole entry is
        /// ignored.
        [0] present,

        /// `R/W`: Allows writes to this memory region.
        [1] writable,

        /// `U/S`: Allows access to this memory region from permission level 3. Otherwise,
        /// it is only accessible from levels 0–2.
        [2] user,

        /// `PWT`: Enables write-through caching for this memory region.
        [3] write_through,

        /// `PCD`: Disables caching for this memory region.
        [4] cache_disabled,

        /// `A`: Set by the processor when an instruction accesses the memory region.
        [5] accessed,

        /// `XD`/`NX`: Prevent the processor from executing any instructions in this
        /// memory region.
        ///
        /// Requires [`ExtendedFeatureEnableRegister::no_execute`].
        #[cfg(any(target_arch = "x86_64", doc))]
        #[doc(cfg(target_arch = "x86_64"))]
        [63] no_execute,
    }
}


/// An page table entry (any level) that directly maps a page.
pub trait DirectPageTableEntry: GenericPageTableEntry {
    bitfield_accessors! {
        /// `D`: Set by the processor when an instruction modifies the memory region.
        [6] dirty,

        /// `PAT`: Used to associate this page with a page attribute table.
        ///
        /// In 32-bit mode, requires [`BasicFeatures::page_attribute_table`]. Always
        /// applicable in 64-bit mode.
        //
        // NOTE: This bit is remapped for implementations of `HybridPageTableEntry`.
        [7] attribute_table,

        /// `G`: Indicates that this is a global page shared by all task contexts.
        ///
        /// Requires [`ControlRegister4::global_pages`].
        [8] global,

        /// The protection key that applies to this memory region.
        ///
        /// Requires [`ControlRegister4::user_protection_keys`] or
        /// [`ControlRegister4::supervisor_protection_keys`].
        #[cfg(any(target_arch = "x86_64", doc))]
        #[doc(cfg(target_arch = "x86_64"))]
        [59..63] protection_key: u8,
    }
}


/// A page table entry (any level) that either directly maps a page or points to another
/// page table.
///
/// This trait provides an implementation for [`DirectPageTableEntry`], but its methods
/// are only applicable if [`is_page`](Self::is_page) is true.
pub trait HybridPageTableEntry: GenericPageTableEntry {
    bitfield_accessors! {
        /// `PS`: Indicates that this entry directly maps a page. Otherwise, this
        /// is a pointer to a lower-level page table.
        ///
        /// In 32-bit mode, requires [`ControlRegister4::page_size_extensions`]. Always
        /// applicable in 64-bit mode.
        [7] is_page,
    }
}

impl<T> DirectPageTableEntry for T
where
    T: HybridPageTableEntry,
{
    bitfield_accessors! {
        // This field is moved in hybrid page tables, because it conflicts with `is_page`.
        [12] attribute_table,
    }
}


bitfield! {
    /// Second-level page table (page directory) entry that either points to a
    /// bottom-level page table or directly maps a 2MB/4MB page.
    pub struct Level2PageTableEntry(usize) {}
}

impl GenericPageTableEntry for Level2PageTableEntry {}
impl HybridPageTableEntry for Level2PageTableEntry {}


bitfield! {
    /// Bottom-level page table entry that maps a single 4KB page.
    pub struct Level1PageTableEntry(usize) {}
}

impl GenericPageTableEntry for Level1PageTableEntry {}
impl DirectPageTableEntry for Level1PageTableEntry {}
