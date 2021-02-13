//! Support for Advanced Configuration and Power Interface
//!
//! See ACPI spec v6.3 (Jan 2019).
// TODO: Handle endianness (all ACPI tables use little-endian)

#![no_std]
#![feature(fn_traits)]
#![warn(clippy::pedantic)]
#![allow(clippy::must_use_candidate)]
#![allow(clippy::upper_case_acronyms)]

extern crate alloc;

use core::mem::size_of;
use static_assertions::const_assert_eq;
use tartan_bitfield::bitfield;
use tartan_c_enum::c_enum;

pub mod aml;


#[repr(C, packed)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
/// Root System Description Pointer (RSDP) used in ACPI 1.0
pub struct RootDescriptionPointerV1 {
    pub signature: [u8; 8],
    pub checksum: u8,
    pub oem_id: [u8; 6],
    pub revision: u8,
    pub root_description_table_addr: u32,
}
const_assert_eq!(20, size_of::<RootDescriptionPointerV1>());


#[repr(C, packed)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
/// Extended RDSP defined by ACPI 2.0
pub struct RootDescriptionPointerV2 {
    pub v1: RootDescriptionPointerV1,
    pub length: u32,
    pub extended_description_table_addr: u64,
    pub extended_checksum: u8,
    _reserved: [u8; 3],
}
const_assert_eq!(36, size_of::<RootDescriptionPointerV2>());


#[repr(C, packed)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
/// Common table header defined as `DESCRIPTION_HEADER` in the spec
pub struct DescriptionHeader {
    pub signature: [u8; 4],
    pub length: u32,
    pub revision: u8,
    pub checksum: u8,
    pub oem_id: [u8; 6],
    pub oem_table_id: [u8; 8],
    pub oem_revision: u32,
    pub creator_id: u32,
    pub creator_revision: u32,
}
const_assert_eq!(36, size_of::<DescriptionHeader>());


#[repr(C, packed)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
/// Fixed ACPI Description Table (FADT)
pub struct FixedDescription {
    pub header: DescriptionHeader,

    pub firmware_api_ctrl_addr_32: u32, // FACS
    pub diff_description_addr_32: u32,  // DSDT

    _reserved_a: u8,

    pub preferred_power_profile: u8, // TODO: enum

    // START ignored in hardware-reduced ACPI
    pub sci_interrupt: u16,
    pub smi_command_port: u32,
    pub smi_command_acpi_enable: u8,
    pub smi_command_acpi_disable: u8,
    pub smi_command_s4_bios_enter: u8,
    pub smi_command_cpu_state_enable: u8,

    pub pm_event_port_block_1a_32: u32,
    pub pm_event_port_block_1b_32: u32,
    pub pm_cntrl_port_block_1a_32: u32,
    pub pm_cntrl_port_block_1b_32: u32,
    pub pm_cntrl_port_block_2_32: u32,
    pub pm_timer_port_block_32: u32,
    pub general_event_port_block_0_32: u32,
    pub general_event_port_block_1_32: u32,
    pub pm_event_port_block_1_length: u8,
    pub pm_cntrl_port_block_1_length: u8,
    pub pm_cntrl_port_block_2_length: u8,
    pub pm_timer_port_block_length: u8,
    pub general_event_port_block_0_length: u8,
    pub general_event_port_block_1_length: u8,
    pub general_event_port_block_1_base: u8,

    pub smi_command_c_state_enable: u8,
    pub cpu_c2_latency: u16,
    pub cpu_c3_latency: u16,

    pub cache_flush_stride_count: u16,
    pub cache_flush_stride_width: u16,

    pub cpu_duty_cycle_register_offset: u8,
    pub cpu_duty_cycle_register_width: u8,

    pub rtc_day_alarm_index: u8,
    pub rtc_month_alarm_index: u8,
    pub rtc_century_alarm_index: u8,

    pub pc_boot_architecture_flags: u16,

    _reserved_b: u8,

    pub flags: FixedFlags,

    pub reset_register: GenericAddress,
    pub reset_register_value: u8,

    pub arm_boot_architecture_flags: u16,

    pub revision_minor: u8,

    pub firmware_api_ctrl_addr_64: u64,
    pub diff_description_addr_64: u64,

    pub pm_event_port_block_1a_64: GenericAddress,
    pub pm_event_port_block_1b_64: GenericAddress,
    pub pm_control_port_block_1a_64: GenericAddress,
    pub pm_control_port_block_1b_64: GenericAddress,
    pub pm_control_port_block_2_64: GenericAddress,
    pub pm_timer_port_block_64: GenericAddress,
    pub general_event_port_block_0_64: GenericAddress,
    pub general_event_port_block_1_64: GenericAddress,
    pub sleep_control_register_addr: GenericAddress,
    pub sleep_status_register_addr: GenericAddress,

    pub hypervisor_vendor: u64,
}
const_assert_eq!(276, size_of::<FixedDescription>());


#[repr(C, packed)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
/// Extended address structure defined in ACPI 2.0 to support 64-bit systems
pub struct GenericAddress {
    address_space: AddressSpace,
    register_bit_width: u8,
    register_bit_offset: u8,
    access_size: AccessSize,
    address: u64,
}
const_assert_eq!(12, size_of::<GenericAddress>());


c_enum! {
    /// Type of register address
    pub enum AddressSpace(u8) {
        SystemMemory                  = 0x00,
        SystemIO                      = 0x01,
        PCIConfiguration              = 0x02,
        EmbeddedController            = 0x03,
        SMBus                         = 0x04,
        SystemCMOS                    = 0x05,
        PCIBARTarget                  = 0x06,
        IPMI                          = 0x07,
        GeneralPurposeIo              = 0x08,
        GenericSerialBus              = 0x09,
        PlatformCommunicationsChannel = 0x0a,

        FunctionalFixed               = 0x7f,

        OEMDefinedMin                 = 0xc0,
        OEMDefinedMax                 = 0xff,
    }
}


c_enum! {
    /// Memory width used to read/write from a register
    pub enum AccessSize(u8) {
        Undefined = 0,
        Byte      = 1,
        Word      = 2,
        DWord     = 3,
        QWord     = 4,
    }
}


bitfield! {
    /// Support flags from Fixed ACPI Description Table
    pub struct FixedFlags(u32) {
        [0] pub x86_wbinvd_supported,
        [1] pub x86_wbinvd_requires_flush,
        [2] pub c1_supported,
        [3] pub c2_multi_cpu_supported,
        [4] pub power_button_is_control_method_device,
        [5] pub sleep_button_is_control_method_device,
        [6] pub rtc_not_fixed,
        [7] pub rtc_can_wake_s4,
        [8] pub timer_value_32_bit,
        [9] pub dock_capable,
        [10] pub fixed_description_reset_supported,
        [11] pub sealed_case,
        [12] pub headless,
        [13] pub execute_instruction_after_slp_typ,
        [14] pub pci_express_wake,
        [15] pub use_platform_clock,
        [16] pub rtc_status_valid_after_s4,
        [17] pub remote_power_on_supported,
        [18] pub force_apic_cluster_model,
        [19] pub force_apic_physical_destination_mode,
        [20] pub hardware_reduced_acpi,
        [21] pub low_power_s0_idle,
    }
}
