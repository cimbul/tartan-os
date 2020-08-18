//! Support for Unified Extensible Firmware Interface
//!
//! See EDK `MdePkg/Include/Uefi/UefiSpec.h`

#![no_std]
#![feature(fn_traits)]
#![feature(ptr_offset_from)]
#![deny(clippy::pedantic)]
#![allow(clippy::must_use_candidate)]

extern crate alloc;

use ::alloc::vec::Vec;
use bitflags::bitflags;
use core::convert::TryInto;
use core::ffi::c_void;
use core::fmt;
use core::mem::{align_of, size_of};
use core::slice;
use crc_any::CRCu32;

pub mod allocator;
pub mod global;
#[macro_use]
pub mod io;

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Handle(usize);

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Revision(pub u32);

#[rustfmt::skip]
impl Revision {
    pub const LATEST: Revision = Revision::V2_80;

    pub const V2_80: Revision = Revision((2 << 16) | 80);
    pub const V2_70: Revision = Revision((2 << 16) | 70);
    pub const V2_60: Revision = Revision((2 << 16) | 60);
    pub const V2_50: Revision = Revision((2 << 16) | 50);
    pub const V2_40: Revision = Revision((2 << 16) | 40);
    pub const V2_31: Revision = Revision((2 << 16) | 31);
    pub const V2_30: Revision = Revision((2 << 16) | 30);
    pub const V2_20: Revision = Revision((2 << 16) | 20);
    pub const V2_10: Revision = Revision((2 << 16) | 10);
    pub const V2_00: Revision = Revision( 2 << 16      );
    pub const V1_10: Revision = Revision((1 << 16) | 10);
    pub const V1_02: Revision = Revision((1 << 16) |  2);
}

impl Revision {
    pub fn major_version(self) -> u16 {
        #![allow(clippy::cast_possible_truncation)]
        (self.0 >> 16) as u16
    }

    pub fn minor_version(self) -> u16 {
        #![allow(clippy::cast_possible_truncation)]
        (self.0 as u16) / 10
    }

    pub fn fix_version(self) -> u16 {
        #![allow(clippy::cast_possible_truncation)]
        (self.0 as u16) % 10
    }
}

impl fmt::Display for Revision {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}.{}.{}",
            self.major_version(),
            self.minor_version(),
            self.fix_version(),
        )
    }
}

#[cfg(test)]
mod test_revision {
    use super::*;

    #[test]
    fn test_components() {
        assert_eq!(Revision::V1_02.major_version(), 1);
        assert_eq!(Revision::V1_02.minor_version(), 0);
        assert_eq!(Revision::V1_02.fix_version(), 2);

        assert_eq!(Revision::V2_31.major_version(), 2);
        assert_eq!(Revision::V2_31.minor_version(), 3);
        assert_eq!(Revision::V2_31.fix_version(), 1);
    }
}

pub type Result = core::result::Result<Status, Status>;

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Status(usize);

#[rustfmt::skip]
impl Status {
    // Appendix D
    pub const SUCCESS: Status = Status(0);

    pub const WARN_UNKNOWN_GLYPH:    Status = Status(1);
    pub const WARN_DELETE_FAILURE:   Status = Status(2);
    pub const WARN_WRITE_FAILURE:    Status = Status(3);
    pub const WARN_BUFFER_TOO_SMALL: Status = Status(4);
    pub const WARN_STALE_DATA:       Status = Status(5);
    pub const WARN_FILE_SYSTEM:      Status = Status(6);
    pub const WARN_RESET_REQUIRED:   Status = Status(7);

    pub const ERROR_BIT: usize = 0x1_usize.reverse_bits(); // high bit
    pub const LOAD_ERROR:           Status = Status(Status::ERROR_BIT | 1);
    pub const INVALID_PARAMETER:    Status = Status(Status::ERROR_BIT | 2);
    pub const UNSUPPORTED:          Status = Status(Status::ERROR_BIT | 3);
    pub const BAD_BUFFER_SIZE:      Status = Status(Status::ERROR_BIT | 4);
    pub const BUFFER_TOO_SMALL:     Status = Status(Status::ERROR_BIT | 5);
    pub const NOT_READY:            Status = Status(Status::ERROR_BIT | 6);
    pub const DEVICE_ERROR:         Status = Status(Status::ERROR_BIT | 7);
    pub const WRITE_PROTECTED:      Status = Status(Status::ERROR_BIT | 8);
    pub const OUT_OF_RESOURCES:     Status = Status(Status::ERROR_BIT | 9);
    pub const VOLUME_CORRUPTED:     Status = Status(Status::ERROR_BIT | 10);
    pub const VOLUME_FULL:          Status = Status(Status::ERROR_BIT | 11);
    pub const NO_MEDIA:             Status = Status(Status::ERROR_BIT | 12);
    pub const MEDIA_CHANGED:        Status = Status(Status::ERROR_BIT | 13);
    pub const NOT_FOUND:            Status = Status(Status::ERROR_BIT | 14);
    pub const ACCESS_DENIED:        Status = Status(Status::ERROR_BIT | 15);
    pub const NO_RESPONSE:          Status = Status(Status::ERROR_BIT | 16);
    pub const NO_MAPPING:           Status = Status(Status::ERROR_BIT | 17);
    pub const TIMEOUT:              Status = Status(Status::ERROR_BIT | 18);
    pub const NOT_STARTED:          Status = Status(Status::ERROR_BIT | 19);
    pub const ALREADY_STARTED:      Status = Status(Status::ERROR_BIT | 20);
    pub const ABORTED:              Status = Status(Status::ERROR_BIT | 21);
    pub const ICMP_ERROR:           Status = Status(Status::ERROR_BIT | 22);
    pub const TFTP_ERROR:           Status = Status(Status::ERROR_BIT | 23);
    pub const PROTOCOL_ERROR:       Status = Status(Status::ERROR_BIT | 24);
    pub const INCOMPATIBLE_VERSION: Status = Status(Status::ERROR_BIT | 25);
    pub const SECURITY_VIOLATION:   Status = Status(Status::ERROR_BIT | 26);
    pub const CRC_ERROR:            Status = Status(Status::ERROR_BIT | 27);
    pub const END_OF_MEDIA:         Status = Status(Status::ERROR_BIT | 28);
    pub const END_OF_FILE:          Status = Status(Status::ERROR_BIT | 31);
    pub const INVALID_LANGUAGE:     Status = Status(Status::ERROR_BIT | 32);
    pub const COMPROMISED_DATA:     Status = Status(Status::ERROR_BIT | 33);
    pub const HTTP_ERROR:           Status = Status(Status::ERROR_BIT | 35);

    pub fn is_error(self) -> bool {
        (self.0 & Status::ERROR_BIT) != 0
    }

    pub fn is_warning(self) -> bool {
        self != Status::SUCCESS && !self.is_error()
    }

    /// Wraps success *and* warning codes in [`Ok`], and error codes in [`Err`].
    #[allow(clippy::missing_errors_doc)]
    pub fn into_result(self) -> Result {
        if self.is_error() {
            Err(self)
        } else {
            Ok(self)
        }
    }
}

impl From<Status> for Result {
    fn from(status: Status) -> Self {
        status.into_result()
    }
}

#[cfg(test)]
mod test_status {
    use super::*;

    #[test]
    fn test_equality() {
        let status = Status(1);
        assert_eq!(status, Status::WARN_UNKNOWN_GLYPH);
        assert_ne!(status, Status::SUCCESS);
        assert_ne!(status, Status::LOAD_ERROR);
    }

    #[test]
    fn test_is_error() {
        assert_eq!(false, Status::SUCCESS.is_error());

        assert_eq!(false, Status::WARN_UNKNOWN_GLYPH.is_error());
        assert_eq!(false, Status::WARN_FILE_SYSTEM.is_error());
        assert_eq!(false, Status::WARN_RESET_REQUIRED.is_error());

        assert_eq!(true, Status::LOAD_ERROR.is_error());
        assert_eq!(true, Status::ACCESS_DENIED.is_error());
        assert_eq!(true, Status::HTTP_ERROR.is_error());

        if cfg!(target_pointer_width = "32") {
            assert_eq!(Status(0x0000_0000).is_error(), false);
            assert_eq!(Status(0x0000_0001).is_error(), false);
            assert_eq!(Status(0x7fff_ffff).is_error(), false);
            assert_eq!(Status(0x8000_0000).is_error(), true);
            assert_eq!(Status(0xffff_ffff).is_error(), true);
        } else {
            assert_eq!(Status(0x0000_0000_0000_0000).is_error(), false);
            assert_eq!(Status(0x0000_0000_0000_0001).is_error(), false);
            assert_eq!(Status(0x7fff_ffff_ffff_ffff).is_error(), false);
            assert_eq!(Status(0x8000_0000_0000_0000).is_error(), true);
            assert_eq!(Status(0xffff_ffff_ffff_ffff).is_error(), true);
        }
    }

    #[test]
    fn test_is_warning() {
        assert_eq!(false, Status::SUCCESS.is_warning());

        assert_eq!(true, Status::WARN_UNKNOWN_GLYPH.is_warning());
        assert_eq!(true, Status::WARN_FILE_SYSTEM.is_warning());
        assert_eq!(true, Status::WARN_RESET_REQUIRED.is_warning());

        assert_eq!(false, Status::LOAD_ERROR.is_warning());
        assert_eq!(false, Status::ACCESS_DENIED.is_warning());
        assert_eq!(false, Status::HTTP_ERROR.is_warning());

        if cfg!(target_pointer_width = "32") {
            assert_eq!(Status(0x0000_0000).is_warning(), false);
            assert_eq!(Status(0x0000_0001).is_warning(), true);
            assert_eq!(Status(0x7fff_ffff).is_warning(), true);
            assert_eq!(Status(0x8000_0000).is_warning(), false);
            assert_eq!(Status(0xffff_ffff).is_warning(), false);
        } else {
            assert_eq!(Status(0x0000_0000_0000_0000).is_warning(), false);
            assert_eq!(Status(0x0000_0000_0000_0001).is_warning(), true);
            assert_eq!(Status(0x7fff_ffff_ffff_ffff).is_warning(), true);
            assert_eq!(Status(0x8000_0000_0000_0000).is_warning(), false);
            assert_eq!(Status(0xffff_ffff_ffff_ffff).is_warning(), false);
        }
    }

    #[test]
    fn test_into_result() {
        assert_eq!(Status::SUCCESS.into_result(), Ok(Status::SUCCESS));
        assert_eq!(Status::WARN_FILE_SYSTEM.into_result(), Ok(Status::WARN_FILE_SYSTEM));
        assert_eq!(Status::LOAD_ERROR.into_result(), Err(Status::LOAD_ERROR));
        assert_eq!(Status::HTTP_ERROR.into_result(), Err(Status::HTTP_ERROR));
    }
}

pub trait Table {
    const SIGNATURE: u64;
    const MIN_REVISION: Revision;

    fn header(&self) -> &TableHeader;

    fn verify(&self)
    where
        Self: Sized,
    {
        self.verify_signature();
        self.verify_revision();
        self.verify_size();
        self.verify_crc32();
    }

    fn verify_signature(&self) {
        let actual_signature = self.header().signature;
        assert!(
            actual_signature == Self::SIGNATURE,
            "Signature mismatch. Expected {:x}, received {:x}",
            actual_signature,
            Self::SIGNATURE,
        )
    }

    fn verify_revision(&self) {
        let actual_revision = self.header().revision;
        assert!(
            actual_revision >= Self::MIN_REVISION,
            "Revision {} older than minimum supported revision {}",
            actual_revision,
            Self::MIN_REVISION,
        )
    }

    fn verify_size(&self)
    where
        Self: Sized,
    {
        let actual_size = self.header().header_size as usize;
        assert!(
            actual_size >= size_of::<Self>(),
            "Header size {} was less than expected {} bytes",
            actual_size,
            size_of::<Self>(),
        )
    }

    fn verify_crc32(&self) {
        const CRC_FIELD_LENGTH: usize = size_of::<u32>();

        let header = self.header();
        let size = header.header_size as usize;
        let start_address = self as *const _ as *const u8;
        let crc_field_address = &header.crc32 as *const _ as *const u8;

        let mut crc = CRCu32::crc32();
        unsafe {
            // Digest start of header until right before CRC value
            crc.digest(slice::from_raw_parts(
                start_address,
                crc_field_address.offset_from(start_address).try_into().unwrap(),
            ));

            // Replace CRC field with zeros
            crc.digest(&[0_u8; CRC_FIELD_LENGTH]);

            // Digest rest of header starting past end of CRC field
            let crc_field_end = crc_field_address.add(CRC_FIELD_LENGTH);
            let end_address = start_address.add(size);
            crc.digest(slice::from_raw_parts(
                crc_field_end,
                end_address.offset_from(crc_field_end).try_into().unwrap(),
            ));
        }

        let orig_remainder = header.crc32;
        let actual_remainder = crc.get_crc();
        assert!(
            actual_remainder == orig_remainder,
            "Calculated CRC {:x} does not match listed value {:x}",
            actual_remainder,
            orig_remainder,
        )
    }
}

#[repr(C)]
pub struct TableHeader {
    pub signature: u64,
    pub revision: Revision,
    pub header_size: u32,
    pub crc32: u32,
    reserved: u32,
}

#[repr(C)]
pub struct SystemTable<'a> {
    pub header: TableHeader,
    pub firmware_vendor: *const u16,
    pub firmware_revision: u32,
    pub console_in_handle: Handle,
    pub console_in: Option<&'a proto::SimpleTextInput>,
    pub console_out_handle: Handle,
    pub console_out: Option<&'a proto::SimpleTextOutput>,
    pub std_err_handle: Handle,
    pub std_err: Option<&'a proto::SimpleTextOutput>,
    pub runtime_services: &'a RuntimeServices,
    pub boot_services: Option<&'a BootServices>,
    pub config_entry_count: usize,
    pub config_table: *const ConfigurationTable,
}

impl Table for SystemTable<'_> {
    const SIGNATURE: u64 = 0x5453_5953_2049_4249;
    const MIN_REVISION: Revision = Revision::V2_00;
    fn header(&self) -> &TableHeader {
        &self.header
    }
}

#[repr(C)]
pub struct RuntimeServices {
    pub header: TableHeader,

    // Time Services
    get_time: usize,
    set_time: usize,
    get_wakeup_time: usize,
    set_wakeup_time: usize,

    // Virtual Memory Services
    set_virtual_address_map: usize,
    convert_pointer: usize,

    // Variable Services
    get_variable: usize,
    get_next_variable_name: usize,
    set_variable: usize,

    // Miscellaneous Services
    get_next_high_monotonic_count: usize,
    reset_system: usize,

    // UEFI 2.0 Capsule Services
    update_capsule: usize,
    query_capsule_capabilities: usize,

    // Miscellaneous UEFI 2.0 Service
    query_variable_info: usize,
}

impl Table for RuntimeServices {
    const SIGNATURE: u64 = 0x5652_4553_544e_5552;
    const MIN_REVISION: Revision = Revision::V2_00;
    fn header(&self) -> &TableHeader {
        &self.header
    }
}

#[repr(C)]
pub struct BootServices {
    pub header: TableHeader,

    // Task Priority Services
    raise_tpl: usize,
    restore_tpl: usize,

    // Memory Services
    // NOTE: Physical addresses are represented as u64 even on 32-bit systems
    pub allocate_pages: unsafe extern "C" fn(
        allocate_type: AllocateType,
        memory_type: MemoryType,
        page_count: usize,
        physical_address: *mut u64,
    ) -> Status,
    pub free_pages:
        unsafe extern "C" fn(physical_address: u64, page_count: usize) -> Status,
    pub get_memory_map: unsafe extern "C" fn(
        map_size: &mut usize,
        map: *mut c_void,
        map_key: &mut usize,
        descriptor_size: &mut usize,
        descriptor_version: &mut u32,
    ) -> Status,
    pub allocate_pool: unsafe extern "C" fn(
        pool_type: MemoryType,
        size: usize,
        buffer: *mut *mut c_void,
    ) -> Status,
    pub free_pool: unsafe extern "C" fn(buffer: *mut c_void) -> Status,

    // Event & Timer Services
    create_event: usize,
    set_timer: usize,
    wait_for_event: usize,
    signal_event: usize,
    close_event: usize,
    check_event: usize,

    // Protocol Handler Services
    install_protocol_interface: usize,
    reinstall_protocol_interface: usize,
    uninstall_protocol_interface: usize,
    pub handle_protocol: unsafe extern "C" fn(
        handle: Handle,
        protocol: &GUID,
        interface: *mut *const c_void,
    ) -> Status,
    reserved: usize,
    register_protocol_notify: usize,
    locate_handle: usize,
    locate_device_path: usize,
    install_configuration_table: usize,

    // Image Services
    load_image: usize,
    start_image: usize,
    exit: usize,
    unload_image: usize,
    exit_boot_services: usize,

    // Miscellaneous Services
    get_next_monotonic_count: usize,
    stall: usize,
    set_watchdog_timer: usize,

    // Driver Support Services
    connect_controller: usize,
    disconnect_controller: usize,

    // Open and Close Protocol Services
    open_protocol: usize,
    close_protocol: usize,
    open_protocol_information: usize,

    // Library Services
    protocols_per_handle: usize,
    locate_handle_buffer: usize,
    locate_protocol: usize,
    install_multiple_protocol_interfaces: usize,
    uninstall_multiple_protocol_interfaces: usize,

    // 32-bit CRC Services
    calculate_crc32: usize,

    // Miscellaneous Services
    copy_mem: usize,
    set_mem: usize,
    create_event_ex: usize,
}

impl Table for BootServices {
    const SIGNATURE: u64 = 0x5652_4553_544f_4f42;
    const MIN_REVISION: Revision = Revision::V2_00;
    fn header(&self) -> &TableHeader {
        &self.header
    }
}

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct GUID(u32, u16, u16, [u8; 8]);

impl GUID {
    pub const fn from(n: u128) -> Self {
        #[allow(clippy::cast_possible_truncation)]
        GUID(
            (n >> 96) as u32,
            (n >> 80) as u16,
            (n >> 64) as u16,
            (n as u64).to_be_bytes(),
        )
    }
}

impl From<u128> for GUID {
    fn from(n: u128) -> Self {
        GUID::from(n)
    }
}

#[cfg(test)]
mod test_guid {
    use super::*;

    #[test]
    fn test_from_u128() {
        let guid = GUID::from(0x01020304_0506_0708_0910_111213141516);
        assert_eq!(guid.0, 0x0102_0304);
        assert_eq!(guid.1, 0x0506);
        assert_eq!(guid.2, 0x0708);
        assert_eq!(guid.3, *b"\x09\x10\x11\x12\x13\x14\x15\x16");
    }

    #[test]
    fn test_equality() {
        let guid_a = GUID::from(0xcf04d973_15f7_400b_b53b_82929911d09c);
        let guid_b = GUID::from(0xcf04d973_15f7_400b_b53b_82929911d09c);
        let guid_c = GUID::from(0x028c338c_0b14_4687_9ad7_14cba520b645);
        assert_eq!(guid_a, guid_b);
        assert_ne!(guid_a, guid_c);
        assert_ne!(guid_b, guid_c);
    }
}

#[repr(C)]
pub struct ConfigurationTable {
    pub vendor_guid: GUID,
    vendor_table: usize,
}

#[rustfmt::skip]
impl ConfigurationTable {
    pub const ACPI_20_GUID:    GUID = GUID::from(0x8868e871_e4f1_11d3_bc22_0080c73c8881);
    pub const ACPI_GUID:       GUID = GUID::from(0xeb9d2d30_2d88_11d3_9a16_0090273fc14d);
    pub const SAL_SYSTEM_GUID: GUID = GUID::from(0xeb9d2d32_2d88_11d3_9a16_0090273fc14d);
    pub const SMBIOS_GUID:     GUID = GUID::from(0xeb9d2d31_2d88_11d3_9a16_0090273fc14d);
    pub const SMBIOS3_GUID:    GUID = GUID::from(0xf2fd1544_9794_4a2c_992e_e5bbcf20e394);
    pub const MPS_GUID:        GUID = GUID::from(0xeb9d2d2f_2d88_11d3_9a16_0090273fc14d);
    // TODO: ... more defined in sect. 4.6
}

#[repr(C)]
pub struct MemoryDescriptor {
    pub memory_type: MemoryType,
    // NOTE: Addresses represented as u64 even on 32-bit systems
    pub physical_start: u64,
    pub virtual_start: u64,
    pub page_count: u64,
    pub attributes: MemoryAttributes,
}

// NOTE: Not strictly part of UEFI API. Might belong in another module.
pub struct MemoryMap {
    pub raw_map: Vec<u8>,
    pub key: usize,
    pub descriptor_size: usize,
    pub descriptor_version: u32,
}

impl MemoryMap {
    pub const MIN_VERSION: u32 = 1;

    pub fn new() -> Self {
        MemoryMap {
            raw_map: Vec::new(),
            key: 0,
            descriptor_size: 0,
            descriptor_version: 0,
        }
    }

    pub fn verify(&self) {
        self.verify_version();
        self.verify_descriptor_size();
        self.verify_map();
    }

    pub fn verify_version(&self) {
        assert!(
            self.descriptor_version >= Self::MIN_VERSION,
            "Descriptor version {} less than required version {}",
            self.descriptor_version,
            Self::MIN_VERSION,
        );
    }

    pub fn verify_descriptor_size(&self) {
        assert!(
            self.descriptor_size >= size_of::<MemoryDescriptor>(),
            "Descriptor size {} shorter than required {} bytes",
            self.descriptor_size,
            size_of::<MemoryDescriptor>(),
        );

        assert!(
            self.descriptor_size % align_of::<MemoryDescriptor>() == 0,
            "Descriptor size {} not a multiple of the MemoryDescriptor struct alignment \
             {}",
            self.descriptor_size,
            align_of::<MemoryDescriptor>(),
        );
    }

    pub fn verify_map(&self) {
        assert!(
            self.raw_map.len() % self.descriptor_size == 0,
            "Memory map total size {} is not a multiple of descriptor size {}",
            self.raw_map.len(),
            self.descriptor_size,
        );

        assert!(
            self.raw_map.as_ptr().align_offset(align_of::<MemoryDescriptor>()) == 0,
            "Memory map at {:p} not aligned properly for dereferencing. Required: {:x}",
            self.raw_map.as_ptr(),
            align_of::<MemoryDescriptor>(),
        );
    }

    /// Iterate over memory descriptors contained in the map.
    pub fn iter<'a>(&'a self) -> impl Iterator<Item = &MemoryDescriptor> + 'a {
        #![allow(clippy::cast_ptr_alignment)]

        // SAFETY: We check the pointer alignment in the verify() call. Since
        // MemoryDescriptor is only composed of unsigned integer types, it is safe to
        // interpret any sequence of bytes as a MemoryDescriptor.
        self.verify();
        self.raw_map
            .as_slice()
            .chunks_exact(self.descriptor_size)
            .map(|raw| unsafe { &*raw.as_ptr().cast::<MemoryDescriptor>() })
    }
}

impl Default for MemoryMap {
    fn default() -> Self {
        Self::new()
    }
}

// TODO: The width of this type really isn't clear. The UEFI spec defines it as a C enum
// with fewer than 256 values. The text refers to reserved ranges up to 0xFFFFFFFF,
// implying that it is at least 32 bits. From what I can tell, MSVC (which UEFI borrows a
// lot of conventions from) uses 32 bits for most enums, even on 64-bit systems.
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MemoryType(u32);

#[rustfmt::skip]
impl MemoryType {
    pub const RESERVED:              MemoryType = MemoryType(0);
    pub const LOADER_CODE:           MemoryType = MemoryType(1);
    pub const LOADER_DATA:           MemoryType = MemoryType(2);
    pub const BOOT_SERVICES_CODE:    MemoryType = MemoryType(3);
    pub const BOOT_SERVICES_DATA:    MemoryType = MemoryType(4);
    pub const RUNTIME_SERVICES_CODE: MemoryType = MemoryType(5);
    pub const RUNTIME_SERVICES_DATA: MemoryType = MemoryType(6);
    pub const CONVENTIONAL:          MemoryType = MemoryType(7);
    pub const UNUSABLE:              MemoryType = MemoryType(8);
    pub const ACPI_RECLAIM:          MemoryType = MemoryType(9);
    pub const ACPI_NVS:              MemoryType = MemoryType(10);
    pub const MAPPED_IO:             MemoryType = MemoryType(11);
    pub const MAPPED_IO_PORT_SPACE:  MemoryType = MemoryType(12);
    pub const PAL_CODE:              MemoryType = MemoryType(13);
    pub const PERSISTENT:            MemoryType = MemoryType(14);
}

// TODO: Same caveat as MemoryType
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AllocateType(u32);

#[rustfmt::skip]
impl AllocateType {
    pub const ANY_ADDRESS:   AllocateType = AllocateType(0);
    pub const MAX_ADDRESS:   AllocateType = AllocateType(1);
    pub const EXACT_ADDRESS: AllocateType = AllocateType(2);
}

bitflags! {
    pub struct MemoryAttributes: u64 {
        const SUPPORTS_UNCACHEABLE     = 0x0000_0000_0000_0001_u64;
        const SUPPORTS_WRITE_COMBINING = 0x0000_0000_0000_0002_u64;
        const SUPPORTS_WRITE_THROUGH   = 0x0000_0000_0000_0004_u64;
        const SUPPORTS_WRITE_BACK      = 0x0000_0000_0000_0008_u64;
        const SUPPORTS_UNCACHEABLE_SEM = 0x0000_0000_0000_0010_u64;
        const SUPPORTS_WRITE_PROTECT   = 0x0000_0000_0000_1000_u64;
        const SUPPORTS_READ_PROTECT    = 0x0000_0000_0000_2000_u64;
        const SUPPORTS_EXEC_PROTECT    = 0x0000_0000_0000_4000_u64;
        const NONVOLATILE              = 0x0000_0000_0000_8000_u64;
        const MORE_RELIABLE            = 0x0000_0000_0001_0000_u64;
        const SUPPORTS_READ_ONLY       = 0x0000_0000_0002_0000_u64;
        const SPECIFIC_PURPOSE         = 0x0000_0000_0004_0000_u64;
        const SUPPORTS_CPU_CRYPTO      = 0x0000_0000_0008_0000_u64;
        const RUNTIME                  = 0x8000_0000_0000_0000_u64;
    }
}

pub mod proto {
    use super::{Handle, MemoryType, Status, SystemTable, GUID};
    use core::ffi::c_void;

    pub trait Protocol {
        const PROTOCOL_ID: GUID;
    }

    #[repr(C)]
    pub struct SimpleTextInput {
        reset: usize,
        read_key_stroke: usize,
        wait_for_key: usize,
        set_state: usize,
        register_key_notify: usize,
        unregister_key_notify: usize,
    }

    #[repr(C)]
    pub struct SimpleTextOutput {
        pub reset: unsafe extern "C" fn(
            this: &SimpleTextOutput,
            extended_verification: bool,
        ) -> Status,
        pub output_string:
            unsafe extern "C" fn(this: &SimpleTextOutput, string: *const u16) -> Status,
        pub test_string:
            unsafe extern "C" fn(this: &SimpleTextOutput, string: *const u16) -> Status,
        query_mode: usize,
        set_mode: usize,
        set_attribute: usize,
        clear_screen: usize,
        set_cursor_position: usize,
        enable_cursor: usize,
        pub mode: SimpleTextOutputMode,
    }

    #[repr(C)]
    pub struct SimpleTextOutputMode {
        pub max_mode: i32,
        pub mode: i32,
        pub attribute: i32,
        pub cursor_column: i32,
        pub cursor_row: i32,
        pub cursor_visible: bool,
    }

    #[repr(C)]
    pub struct LoadedImage<'a> {
        pub revision: u32,
        pub parent_handle: Handle,
        pub system_table: *const SystemTable<'a>,

        pub device_handle: Handle,
        device_path_protocol: usize, // TODO: type
        _reserved: usize,

        pub load_options_size: u32,
        pub load_options: *const c_void,

        pub image_base: *mut c_void,
        pub image_size: u64,
        pub image_code_type: MemoryType,
        pub image_data_type: MemoryType,
        pub unload: unsafe extern "C" fn(handle: Handle) -> Status,
    }

    impl Protocol for LoadedImage<'_> {
        const PROTOCOL_ID: GUID = GUID::from(0x5b1b31a1_9562_11d2_8e3f_00a0c969723b);
    }

    impl LoadedImage<'_> {
        pub const LATEST_REVISION: u32 = 0x1000;
    }
}
