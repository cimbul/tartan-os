//! Processor feature detection with CPUID.

use tartan_bitfield::bitfield;


/// Low-level CPUID call. Writes the leaf/subleaf index given in the arguments to EAX/ECX
/// and returns the values written to EAX–EDX.
pub fn cpuid(leaf_index: u32, subleaf_index: u32) -> [u32; 4] {
    let mut result = [0_u32; 4];
    unsafe {
        asm!(
            "cpuid",
            inout("eax") leaf_index => result[0],
            out("ebx") result[1],
            inout("ecx") subleaf_index => result[2],
            out("edx") result[3],
        );
    }
    result
}

/// Get the maximum primary index value (EAX) supported for *basic* CPUID calls by this
/// processor (below `0x8000_0000`).
pub fn max_cpuid_index_basic() -> u32 {
    cpuid(0, 0)[0]
}

/// Get the maximum primary index value (EAX) supported for *extended* CPUID calls by this
/// processor (above `0x8000_0000`).
pub fn max_cpuid_index_extended() -> u32 {
    cpuid(0x8000_0000, 0)[0]
}


bitfield! {
    /// Primary feature list returned in `CPUID.01H:ECX+EDX`.
    pub struct Features(u64) {
        // Bits 0..32 from EDX

        /// `FPU`: The processor has a built-in x87 floating-point unit
        [ 0] pub on_chip_fpu,
        /// `VME`: Supports virtual real-mode extensions (VME) and protected-mode virtual
        /// interrupts.
        [ 1] pub virtual_8086_extensions,
        /// `DE`: Supports breaking on I/O and on accessing debug registers `DR4`–`DR5`.
        [ 2] pub debugging_extensions,
        /// `PSE`: Supports 4MB virtual memory pages and the dirty flag.
        [ 3] pub page_size_extension,
        /// `TSC`: Supports reading the processor's timestamp with `RDTSC`.
        [ 4] pub time_stamp_counter,
        /// `MSR`: Has model-specific registers which can be accessed with
        /// `RDMSR`/`WRMSR`.
        [ 5] pub model_registers,
        /// `PAE`: Supports mapping virtual memory to physical addresses longer than 32
        /// bits.
        [ 6] pub physical_address_extension,
        /// `MCE`: Defines an exception (18) for reporting internal processor errors.
        [ 7] pub machine_check_exception,
        /// `CX8`: Supports the 64-byte `CMPXCHG8B` atomic instruction.
        [ 8] pub compare_exchange_64bit,
        /// `APIC`: The processor has a built-in advanced programmable interrupt
        /// controller (APIC).
        [ 9] pub on_chip_apic,
        /// `SEP`: Supports the `SYSENTER`/`SYSEXIT` instructions.
        [11] pub sysenter,
        /// `MTRR`: Has memory type range registers.
        [12] pub memory_type_range_registers,
        /// `PGE`: Supports global pages, which are available in all task contexts
        [13] pub global_pages,
        /// `MCA`: Supports extended features for reporting internal processor errors.
        [14] pub machine_check_architecture,
        /// `CMOV`: Supports the `CMOV` instruction and `FCMOV`/`FCOMI` if FPU is present.
        [15] pub conditional_move,
        /// `PAT`: Supports page attribute tables.
        [16] pub page_attribute_table,
        /// `PSE-36`: Supports 4MB virtual memory pages that can map to physical addresses
        /// longer than 32 bits.
        [17] pub page_size_extension_36bit,
        /// `PSN`: Supports retrieving a processor serial number with the CPUID
        /// instruction.
        [18] pub serial_number,
        /// `CLFSH`: Supports flushing a cache line with the `CLFLUSH` instruction.
        [19] pub cache_line_flush,
        /// `DS`: Supports writing debug information to memory.
        [21] pub debug_store,
        /// `ACPI`: Supports thermal monitoring and power management with software.
        [22] pub thermal_power_management,
        /// `MMX`: Supports MMX instructions.
        [23] pub mmx,
        /// `FXSAVE`: Supports managing FPU state with `FXSAVE`/`FXRSTOR`.
        [24] pub fpu_save,
        /// `SSE`: Supports SSE instructions.
        [25] pub sse,
        /// `SSE2`: Supports SSE2 instructions.
        [26] pub sse_2,
        /// The processor can snoop on its own cache line. This helps deal with certain
        /// memory issues.
        [27] pub self_snoop,
        /// `HTT`: Indicates that the number of reserved APIC IDs is available with the
        /// CPUID instruction. If clear, only one ID is reserved.
        [28] pub max_apic_id_field,
        /// `TM`: Has thermal monitor control circuitry (TCC).
        [29] pub thermal_monitor,
        /// `PBE`: Supports a pin notifying a stopped processor that an interrupt is
        /// pending.
        [31] pub pending_break_enable,


        // Bits 32..64 from ECX

        /// `SSE3`: Supports SSE3 instructions.
        [32] pub sse_3,
        /// Supports carry-less multiplication of two 64-bit integers using the
        /// `PCLMULQDQ` instruction.
        [33] pub carryless_multiply_64bit,
        /// `DTES64`: Supports 64-bit addresses for the debug store.
        [34] pub debug_store_64bit,
        /// `MONITOR`: Supports the `MONITOR`/`MWAIT` instructions.
        [35] pub monitor,
        /// `DS-CPL`: Supports saving the permission level with data written to the debug
        /// store.
        [36] pub permission_qualified_debug_store,
        /// `VMX`: Supports virtual machine extensions.
        [37] pub virtual_machine_extensions,
        /// `SMX`: Supports safer-mode extensions
        [38] pub safer_mode_extensions,
        /// `EIST`: Supports enhanced SpeedStep throttling.
        [39] pub enhanced_speedstep,
        /// Supports the TM2 thermal monitor interface.
        [40] pub thermal_monitor_2,
        /// `SSSE3`: Supports Supplemental SSE3 (SSSE3) instructions.
        [41] pub supplemental_sse_3,
        /// `CNXT-ID`: Supports setting the L1 cache to adaptive or shared mode.
        [42] pub l1_context_id,
        /// `SDBG`: Supports an MSR for chip debugging.
        [43] pub debug_interface_model_register,
        /// `FMA`: Supports fused multiply-add SSE instructions.
        [44] pub fused_multiply_add,
        /// `CMPXCHG16B`: Supports the 128-bit `CMPXCHG16B` atomic instruction.
        [45] pub compare_exchange_128bit,
        /// Supports disabling xTPR task priority messages to the chipset through
        /// `IA32_MISC_ENABLE[23]`.
        [46] pub chipset_task_priority_control,
        /// `PDCM`: Supports a model-specific register that lists performance-monitoring
        /// and debug features.
        [47] pub monitor_debug_capabilities_register,
        /// `PCID`: Supports process-context IDs.
        [49] pub process_context_ids,
        /// `DCA`: Supports prefetching memory-mapped data from a device.
        [50] pub memory_mapped_prefetch,
        /// `SSE4_1`: Supports SSE4.1 instructions.
        [51] pub sse_4_1,
        /// `SSE4_2`: Supports SSE4.2 instructions.
        [52] pub sse_4_2,
        /// `x2APIC`: Supports the enhanced "x2" interface for the APIC.
        [53] pub apic_x2,
        /// Supports byte swapping with the `MOVBE` instruction.
        [54] pub byte_swap_move,
        /// Supports counting the set bits in a value with the `POPCNT` instruction.
        [55] pub count_bits,
        /// `TSC-Deadline`: Supports one-shot interrupts with the APIC using the timestamp
        /// counter.
        [56] pub apic_timestamp_deadline,
        /// `AESNI`: Supports AES acceleration instructions.
        [57] pub aes,
        /// `XSAVE`: Supports instructions for saving and restoring extended processor
        /// state (FPU/MMX/SSE/AVX).
        [58] pub extended_state_save,
        /// `OSXSAVE`: Reflects the value of [`ControlRegister4::extended_state_save`],
        /// indicating that the OS has enabled the `XSAVE` feature.
        [59] pub extended_state_save_enabled,
        /// `AVX`: Supports AVX instructions.
        [60] pub avx,
        /// `F16C`: Supports conversion instructions for 16-bit floats.
        [61] pub float_16_conversion,
        /// Supports random number generation with the `RDRAND` instruction.
        [62] pub random,
    }
}

impl Features {
    /// Retrieve the feature list from the processor using the CPUID instruction.
    pub fn get() -> Self {
        let result = cpuid(1, 0);
        // Put ECX (result[2]) in high DWord, EDX (result[3]) in low. The features in EDX
        // are older and more basic, so it makes sense to start the bit numbering with
        // those.
        let mut features = Self(0);
        features.0 |= u64::from(result[3]);
        features.0 |= u64::from(result[2]) << 32;
        features
    }
}


bitfield! {
    /// Features applicable to 64-bit processors, returned in `CPUID.80000000H:ECX+EDX`.
    pub struct ExtendedFeatures(u64) {
        // Bits 0..32 from EDX

        /// Supports `SYSCALL`/`SYSRET` instructions. Always false if the processor is not
        /// in 64-bit mode.
        [11] syscall,
        /// Supports no-execute (NX) bit in virtual memory pages.
        [20] no_execute_bit,
        /// Supports 1GB pages.
        [26] page_size_1gb,
        /// Supports reading the processor ID along with the timestamp counter using the
        /// `RDTSCP` instruction.
        [27] timestamp_with_processor,
        /// Supports 64-bit mode.
        [29] long_mode,


        // Bits 32..64 from ECX

        /// Supports the `LAHF` instruction in 64-bit mode.
        [32] long_mode_ah_flags,
        /// Supports counting leading zero bits with the `LZCNT` instruction.
        [37] count_leading_zeros,
        /// Supports hinting pending writes with the `PREFETCHW` instruction.
        [40] prefetch_for_write,
    }
}

impl ExtendedFeatures {
    /// Retrieve the extended feature list from the processor using the CPUID instruction.
    pub fn get() -> Self {
        let result = cpuid(0x8000_0001, 0);
        // Same ordering rationale as with Features
        let mut features = Self(0);
        features.0 |= u64::from(result[3]);
        features.0 |= u64::from(result[2]) << 32;
        features
    }
}


bitfield! {
    /// Indicates the processor's maximum supported physical and virtual address sizes.
    pub struct AddressSpaceSizes(u32) {
        /// Maximum number of bits supported in physical addresses.
        [ 0.. 8] physical_address_bits: u8,
        /// Maximum number of bits supported in virtual (linear) addresses.
        [ 8..16] virtual_address_bits: u8,
    }
}

impl AddressSpaceSizes {
    /// Retrieve the supported address space sizes from the processor using the CPUID
    /// instruction.
    pub fn get() -> Self {
        let index = 0x8000_0008;
        if index <= max_cpuid_index_extended() {
            Self(cpuid(index, 0)[0])
        } else {
            // Assume 32 bits for both
            let mut result = Self::default();
            result.set_physical_address_bits(32);
            result.set_virtual_address_bits(32);
            result
        }
    }
}
