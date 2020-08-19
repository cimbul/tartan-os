//! Support for Advanced Configuration and Power Interface
//!
//! See ACPI spec v6.3 (Jan 2019).
// TODO: Handle endianness (all ACPI tables use little-endian)

#![no_std]
#![feature(fn_traits)]
#![feature(ptr_offset_from)]
#![deny(clippy::pedantic)]
#![allow(clippy::must_use_candidate)]

extern crate alloc;

use bitflags::bitflags;
use core::mem::size_of;
use static_assertions::const_assert_eq;

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


#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
/// Type of register address
pub struct AddressSpace(u8);

#[rustfmt::skip]
impl AddressSpace {
    pub const SYSTEM_MEMORY:                   AddressSpace = AddressSpace(0x00);
    pub const SYSTEM_IO:                       AddressSpace = AddressSpace(0x01);
    pub const PCI_CONFIGURATION:               AddressSpace = AddressSpace(0x02);
    pub const EMBEDDED_CONTROLLER:             AddressSpace = AddressSpace(0x03);
    pub const SM_BUS:                          AddressSpace = AddressSpace(0x04);
    pub const SYSTEM_CMOS:                     AddressSpace = AddressSpace(0x05);
    pub const PCI_BAR_TARGET:                  AddressSpace = AddressSpace(0x06);
    pub const IPMI:                            AddressSpace = AddressSpace(0x07);
    pub const GENERAL_PURPOSE_IO:              AddressSpace = AddressSpace(0x08);
    pub const GENERIC_SERIAL_BUS:              AddressSpace = AddressSpace(0x09);
    pub const PLATFORM_COMMUNICATIONS_CHANNEL: AddressSpace = AddressSpace(0x0A);

    pub const FUNCTIONAL_FIXED:                AddressSpace = AddressSpace(0x7F);

    pub const OEM_DEFINED_MIN:                 AddressSpace = AddressSpace(0xC0);
    pub const OEM_DEFINED_MAX:                 AddressSpace = AddressSpace(0xFF);
}


#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
/// Memory width used to read/write from a register
pub struct AccessSize(u8);

#[rustfmt::skip]
impl AccessSize {
    pub const UNDEFINED: AccessSize = AccessSize(0);
    pub const BYTE:      AccessSize = AccessSize(1);
    pub const WORD:      AccessSize = AccessSize(2);
    pub const DWORD:     AccessSize = AccessSize(3);
    pub const QWORD:     AccessSize = AccessSize(4);
}


bitflags! {
    /// Support flags from Fixed ACPI Description Table
    pub struct FixedFlags: u32 {
        const X86_WBINVD_SUPPORTED                  = 1;
        const X86_WBINVD_REQUIRES_FLUSH             = 1 << 1;
        const C1_SUPPORTED                          = 1 << 2;
        const C2_MULTI_CPU_SUPPORTED                = 1 << 3;
        const POWER_BUTTON_IS_CONTROL_METHOD_DEVICE = 1 << 4;
        const SLEEP_BUTTON_IS_CONTROL_METHOD_DEVICE = 1 << 5;
        const RTC_NOT_FIXED                         = 1 << 6;
        const RTC_CAN_WAKE_S4                       = 1 << 7;
        const TIMER_VALUE_32_BIT                    = 1 << 8;
        const DOCK_CAPABLE                          = 1 << 9;
        const FIXED_DESCRIPTION_RESET_SUPPORTED     = 1 << 10;
        const SEALED_CASE                           = 1 << 11;
        const HEADLESS                              = 1 << 12;
        const EXECUTE_INSTRUCTION_AFTER_SLP_TYP     = 1 << 13;
        const PCI_EXPRESS_WAKE                      = 1 << 14;
        const USE_PLATFORM_CLOCK                    = 1 << 15;
        const RTC_STATUS_VALID_AFTER_S4             = 1 << 16;
        const REMOTE_POWER_ON_SUPPORTED             = 1 << 17;
        const FORCE_APIC_CLUSTER_MODEL              = 1 << 18;
        const FORCE_APIC_PHYSICAL_DESTINATION_MODE  = 1 << 19;
        const HARDWARE_REDUCED_ACPI                 = 1 << 20;
        const LOW_POWER_S0_IDLE                     = 1 << 21;
    }
}
