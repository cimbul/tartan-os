//! [`BootServices`](super::BootServices)-based heap allocator to support the standard
//! [alloc] crate.

use core::alloc::{GlobalAlloc, Layout};
use core::ffi::c_void;
use core::ptr;
use super::MemoryType;
use super::global::SYSTEM_TABLE;

pub struct BootAllocator;

impl BootAllocator {
    // UEFI spec guarantees 8-byte alignment from allocate_pool()
    const UEFI_ALIGNMENT: usize = 8;

    const VOID_PTR_LAYOUT: Layout = Layout::new::<*const c_void>();

    fn extend_layout_for_alignment(orig_layout: Layout) -> (Layout, usize) {
        // To manually align the memory region, so we need extra space for:
        //  * A place to store the original pointer that allocate_pool() gave us, so that
        //    we can recover it for free_pool()
        //  * One extra unit of alignment to accommodate any shifting we need to do to
        //    align the pointer
        let (saved_ptr_layout, saved_ptr_offset) =
            orig_layout.extend(Self::VOID_PTR_LAYOUT)
                .expect("Could not construct extended layout for alignment structure");
        let adjusted_size = saved_ptr_layout.size() + saved_ptr_layout.align();
        let adjusted_layout =
            Layout::from_size_align(adjusted_size, saved_ptr_layout.align())
                .expect("Could not construct extended layout for alignable allocation");
        (adjusted_layout, saved_ptr_offset)
    }

    unsafe fn alloc(layout: Layout) -> *mut u8 {
        let system_table = SYSTEM_TABLE
            .expect("System table not initialized");
        let boot_services = (*system_table).boot_services
            .expect("Boot services unavailable");

        let (adjusted_layout, saved_ptr_offset) =
            if layout.align() <= Self::UEFI_ALIGNMENT {
                // allocate_pool() will give the proper alignment. No adjustment needed.
                (layout, 0)
            } else {
                Self::extend_layout_for_alignment(layout)
            };

        let mut buffer: *mut c_void = ptr::null_mut();
        (boot_services.allocate_pool)(MemoryType::LOADER_DATA,
            adjusted_layout.size(),
            &mut buffer,
        ).into_result().expect("allocate_pool() failed");

        if adjusted_layout.align() > Self::UEFI_ALIGNMENT {
            let orig_address = buffer as usize;
            let align_offset = orig_address % adjusted_layout.align();
            if align_offset != 0 {
                // Shift to accommodate alignment
                let shifted_ptr = orig_address + (adjusted_layout.align() - align_offset);
                buffer = shifted_ptr as *mut c_void;
            }
            // Store the original pointer after the object we are allocating. This happens
            // even if we didn't have make an alignment shift, because dealloc() has no
            // way of telling whether we did.
            let saved_ptr_ptr = (buffer as usize + saved_ptr_offset) as *mut *mut c_void;
            *saved_ptr_ptr = orig_address as *mut c_void;
        }

        buffer.cast()
    }

    unsafe fn dealloc(buffer: *mut u8, layout: Layout) {
        let system_table = SYSTEM_TABLE
            .expect("System table not initialized");
        let boot_services = (*system_table).boot_services
            .expect("Boot services unavailable");

        let original_ptr: *mut c_void = if layout.align() <= Self::UEFI_ALIGNMENT {
            // We gave Rust the pointer that came from allocate_pool() directly
            buffer.cast()
        } else {
            // This pointer was manually aligned, which means it isn't the same as the
            // pointer that allocate_pool() gave us. We have to recover the original
            // pointer to pass to free_pool().
            let (_, saved_ptr_offset) = Self::extend_layout_for_alignment(layout);
            let saved_ptr_ptr = (buffer as usize + saved_ptr_offset) as *mut *mut c_void;
            *saved_ptr_ptr
        };

        (boot_services.free_pool)(original_ptr).into_result()
            .expect("Could not free memory");
    }
}

unsafe impl GlobalAlloc for BootAllocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        BootAllocator::alloc(layout)
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        BootAllocator::dealloc(ptr, layout);
    }
}
