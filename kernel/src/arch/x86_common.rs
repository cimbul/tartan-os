//! Shared architecture-specific bindings for 32-bit and 64-bit Intel x86-based processors

use core::arch::naked_asm;
use core::mem::size_of;
use core::ptr::addr_of;
use memoffset::offset_of;
use paste::paste;
use tartan_arch::x86_common::interrupt::{
    InterruptDescriptorTableRegister, InterruptVector,
};
use tartan_arch::x86_common::protection::{
    DescriptorFlags, GateDescriptor, GateDescriptorFlags, GlobalDescriptorTableRegister,
    LocalDescriptorTableRegister, SegmentDescriptor, SegmentDescriptorFlags, Selector,
    SystemDescriptorType, TaskRegister,
};
use tartan_arch::x86_common::{ControlRegister0, ControlRegister4, FlagRegister};

#[cfg(target_arch = "x86")]
use tartan_arch::x86::protection::TaskStateSegmentHeader;

#[cfg(target_arch = "x86_64")]
use tartan_arch::x86_64::protection::TaskStateSegmentHeader;


static mut GLOBAL_DESCRIPTOR_TABLE: GlobalDescriptorTable = GlobalDescriptorTable {
    null_segment: SegmentDescriptor::new(),
    code_segment: SegmentDescriptor::new(),
    data_segment: SegmentDescriptor::new(),
    task_state_segment: SegmentDescriptor::new(),
};

static mut INTERRUPT_DESCRIPTOR_TABLE: InterruptDescriptorTable =
    InterruptDescriptorTable { descriptors: [GateDescriptor::new(); 0xff] };

static mut TASK_STATE_SEGMENT: TaskStateSegmentHeader = TaskStateSegmentHeader::new();


/// Global descriptor table structure specific to Tartan OS.
struct GlobalDescriptorTable {
    /// Dummy descriptor for segment index 0. The processor always treats index 0 as a
    /// null value, so this descriptor is ignored.
    pub null_segment: SegmentDescriptor,
    /// Main code segment that covers all memory, used by CS.
    pub code_segment: SegmentDescriptor,
    /// Main data that covers all memory, used by everything but CS.
    pub data_segment: SegmentDescriptor,
    /// Sole task state segment required by task register.
    pub task_state_segment: SegmentDescriptor,
}

/// Create a segment selector that points to the given descriptor field in the
/// [`GlobalDescriptorTable`].
macro_rules! global_selector {
    [$segment:ident] => {
        {
            let offset = offset_of!(GlobalDescriptorTable, $segment);
            Selector::new(offset.try_into().unwrap(), 0, false)
        }
    }
}


struct InterruptDescriptorTable {
    pub descriptors: [GateDescriptor; 0xff],
}


/// Set flags in general control registers to a known state
pub fn initialize_control_registers() {
    let mut flags = FlagRegister::get();
    assert!(!flags.virtual_8086_mode()); // Firmware/loader should have disabled
    flags.set_interrupt_enabled(true); // Should be harmless... maybe
    flags.set_io_privilege_level(0); // Basic security
    flags.set_alignment_check_or_access_control(true); // Check for Rust UB/extra security
    unsafe {
        FlagRegister::set(flags);
    }

    let mut cr0 = ControlRegister0::get();
    assert!(cr0.protected_mode()); // Firmware/loader should have enabled
    cr0.set_cache_disabled(false); // Should be harmless
    cr0.set_cache_not_write_through(false); // Should be harmless
    cr0.set_alignment_check_mask(true); // Check for Rust UB
    cr0.set_write_protect(true); // Basic security
    cr0.set_native_fpu_error(true); // Don't use MS-DOS compatibility mode
    cr0.set_fpu_emulation(true);
    #[cfg(target_arch = "x86_64")]
    {
        assert!(cr0.paging()); // Required in 64-bit mode
    }
    unsafe {
        ControlRegister0::set(cr0);
    }

    let mut cr4 = ControlRegister4::get();
    cr4.set_virtual_8086_extensions(false); // No reason to support this
    cr4.set_protected_virtual_interrupts(false); // No reason to support this
    cr4.set_timestamp_disabled(true); // Extra security, no use case yet
    cr4.set_debugging_extensions(true); // More modern, should be harmless
    cr4.set_page_size_extensions(false); // Not used yet
    cr4.set_machine_check_exception(true); // Should be harmless
    cr4.set_global_pages(false); // Not used yet
    cr4.set_performance_counter(false); // Extra security, no use case yet
    cr4.set_sse_and_fpu_save(false); // Need to write support
    cr4.set_simd_exceptions(false); // Need to write support
    cr4.set_restrict_user_mode_instructions(false); // TODO: Enable for extra security
    cr4.set_virtual_machine_extensions(false); // Intel-only, not used yet
    cr4.set_safer_mode_extensions(false); // Intel-only, poor implementation
    cr4.set_extended_state_save(false); // Need to write support
    cr4.set_supervisor_execution_prevention(false); // TODO: Enable for extra security
    cr4.set_supervisor_access_prevention(false); // TODO: Enable for extra security
    cr4.set_control_flow_enforcement(false); // Intel-only, not used yet
    #[cfg(target_arch = "x86_64")]
    {
        assert!(cr4.physical_address_extension()); // Required in 64-bit mode
        cr4.set_five_level_paging(false); // Not used yet
        cr4.set_extended_base_registers(false); // Not used yet
        cr4.set_process_context_ids(false); // Not used yet
        cr4.set_user_protection_keys(false); // Not used yet
        cr4.set_supervisor_protection_keys(false); // Intel-only, not used yet
    }
    unsafe {
        ControlRegister4::set(cr4);
    }

    #[cfg(target_arch = "x86_64")]
    {
        use tartan_arch::x86_64::ExtendedFeatureEnableRegister;

        let mut efer = ExtendedFeatureEnableRegister::get();
        assert!(efer.long_mode_active()); // How else are we here?
        efer.set_syscall(true); // Most efficient method for syscalls
        efer.set_no_execute(true); // Extra security
        unsafe {
            ExtendedFeatureEnableRegister::set(efer);
        }
    }
}


/// Initialize the segmentation system to use a flat memory model pointing to our
/// [`GlobalDescriptorTable`].
pub fn initialize_segments() {
    // Initialize the segment descriptors in our global descriptor table
    unsafe {
        GLOBAL_DESCRIPTOR_TABLE.code_segment = make_code_descriptor();
        GLOBAL_DESCRIPTOR_TABLE.data_segment = make_data_descriptor();
        GLOBAL_DESCRIPTOR_TABLE.task_state_segment = make_task_state_descriptor();
    }

    // Set the GDTR to use our global descriptor table, and point the segment selectors
    // (CS, DS, SS, etc.) to the new descriptors.
    let gdtr = GlobalDescriptorTableRegister {
        address: addr_of!(GLOBAL_DESCRIPTOR_TABLE) as usize,
        limit: (size_of::<GlobalDescriptorTable>() - 1).try_into().unwrap(),
    };
    unsafe {
        GlobalDescriptorTableRegister::set_with_segments(
            &gdtr,
            global_selector!(code_segment),
            global_selector!(data_segment),
        )
    }

    // Point the task register to our task state segment
    unsafe {
        TaskRegister::set(global_selector!(task_state_segment));
    }

    // We don't use local descriptors, so point the LDTR to the null descriptor
    unsafe {
        LocalDescriptorTableRegister::set(global_selector!(null_segment));
    }
}

fn make_code_descriptor() -> SegmentDescriptor {
    #![allow(clippy::field_reassign_with_default)] // Spurious: constructor is private

    let mut flags = SegmentDescriptorFlags::default();
    flags.set_present(true);
    flags.set_granularity(true);
    flags.set_is_application(true);
    flags.set_is_code(true);
    flags.set_code_readable(true);

    #[cfg(target_arch = "x86")]
    flags.set_application_mode_32(true);

    #[cfg(target_arch = "x86_64")]
    flags.set_code_mode_64(true);

    let mut descriptor = SegmentDescriptor::default();
    descriptor.flags = flags;
    descriptor.set_limit(SegmentDescriptor::LIMIT_MAX);
    descriptor
}

fn make_data_descriptor() -> SegmentDescriptor {
    #![allow(clippy::field_reassign_with_default)] // Spurious: constructor is private

    let mut flags = SegmentDescriptorFlags::default();
    flags.set_present(true);
    flags.set_granularity(true);
    flags.set_is_application(true);
    flags.set_is_code(false);
    flags.set_data_writable(true);

    #[cfg(target_arch = "x86")]
    flags.set_application_mode_32(true);

    let mut descriptor = SegmentDescriptor::default();
    descriptor.flags = flags;
    descriptor.set_limit(SegmentDescriptor::LIMIT_MAX);
    descriptor
}

fn make_task_state_descriptor() -> SegmentDescriptor {
    #![allow(clippy::field_reassign_with_default)] // Spurious: constructor is private

    let mut flags = SegmentDescriptorFlags::default();
    flags.set_present(true);
    flags.set_is_application(false);
    flags.set_system_type(SystemDescriptorType::TaskStateAvailable);

    let mut descriptor = SegmentDescriptor::default();
    descriptor.flags = flags;
    descriptor.set_address(addr_of!(TASK_STATE_SEGMENT) as usize);
    descriptor.set_limit((size_of::<TaskStateSegmentHeader>() - 1).try_into().unwrap());
    descriptor
}


pub fn initialize_interrupts() {
    /// Create a function that forwards the given interrupt number to the
    /// [`handle_unknown_interrupt`] function, and point the appropriate IDT entry to it.
    macro_rules! forward_interrupt {
        [$vector:literal] => {
            paste! {
                #[unsafe(naked)]
                unsafe extern "C" fn [< forward_interrupt_ $vector >]() {
                    // TODO: Save register state before calling handler, and support
                    // returning. This isn't critical at the moment since the handler just
                    // panics.

                    #[cfg(target_arch = "x86")]
                    naked_asm!(
                        "
                        push {}
                        call {}
                        ",
                        const $vector,
                        sym handle_unknown_interrupt,
                    );

                    #[cfg(target_arch = "x86_64")]
                    naked_asm!(
                        "
                        mov rdi, {}
                        call {}
                        ",
                        const $vector,
                        sym handle_unknown_interrupt,
                    );
                }

                unsafe {
                    INTERRUPT_DESCRIPTOR_TABLE.descriptors[$vector] =
                        make_interrupt_gate(
                            global_selector!(code_segment),
                            [< forward_interrupt_ $vector >],
                        );
                }
            }
        }
    }

    // Set up the interrupt gates in our IDT
    forward_interrupt!(0);
    forward_interrupt!(1);
    forward_interrupt!(2);
    forward_interrupt!(3);
    forward_interrupt!(4);
    forward_interrupt!(5);
    forward_interrupt!(6);
    forward_interrupt!(7);
    forward_interrupt!(8);
    forward_interrupt!(9);
    forward_interrupt!(10);
    forward_interrupt!(11);
    forward_interrupt!(12);
    forward_interrupt!(13);
    forward_interrupt!(14);
    forward_interrupt!(15);
    forward_interrupt!(16);
    forward_interrupt!(17);
    forward_interrupt!(18);
    forward_interrupt!(19);
    forward_interrupt!(20);
    forward_interrupt!(21);
    forward_interrupt!(22);
    forward_interrupt!(23);
    forward_interrupt!(24);
    forward_interrupt!(25);
    forward_interrupt!(26);
    forward_interrupt!(27);
    forward_interrupt!(28);
    forward_interrupt!(29);
    forward_interrupt!(30);
    forward_interrupt!(31);

    // Point the IDTR to our IDT
    let idtr = InterruptDescriptorTableRegister {
        address: addr_of!(INTERRUPT_DESCRIPTOR_TABLE) as usize,
        limit: (size_of::<InterruptDescriptorTable>() - 1).try_into().unwrap(),
    };
    unsafe { InterruptDescriptorTableRegister::set(&idtr) };
}

fn make_interrupt_gate(
    code_segment: Selector,
    handler: unsafe extern "C" fn(),
) -> GateDescriptor {
    #![allow(clippy::field_reassign_with_default)] // Spurious: constructor is private

    let mut flags = GateDescriptorFlags::default();
    flags.set_present(true);
    flags.set_is_application(false);
    flags.set_system_type(SystemDescriptorType::InterruptGate);

    let mut descriptor = GateDescriptor::default();
    descriptor.flags = flags;
    descriptor.set_selector(code_segment);
    descriptor.set_entry_point_offset(handler as usize);
    descriptor
}

fn handle_unknown_interrupt(vector: InterruptVector) {
    panic!("Interrupt {:?}", vector);
}
