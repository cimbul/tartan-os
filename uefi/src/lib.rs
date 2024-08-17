//! Support for Unified Extensible Firmware Interface
//!
//! See EDK `MdePkg/Include/Uefi/UefiSpec.h`

#![no_std]
#![feature(fn_traits)]
#![warn(clippy::pedantic)]
#![allow(clippy::must_use_candidate)]
#![allow(clippy::non_ascii_literal)]
#![allow(clippy::upper_case_acronyms)]

extern crate alloc;

use ::alloc::vec::Vec;
use core::ffi::c_void;
use core::fmt;
use core::mem::{align_of, size_of};
use core::slice;
use crc_any::CRCu32;
use tartan_bitfield::bitfield;
use tartan_c_enum::c_enum;

pub mod allocator;
pub mod global;
#[macro_use]
pub mod io;


/// Constant page size defined by UEFI specification for [`BootServices::allocate_pages`].
pub const PAGE_SIZE: usize = 4096;


#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Handle(usize);

impl Handle {
    pub const NULL: Handle = Handle(0);
}


c_enum! {
    pub enum Revision(u32) {
        V2_80 = (2 << 16) | 80,
        V2_70 = (2 << 16) | 70,
        V2_60 = (2 << 16) | 60,
        V2_50 = (2 << 16) | 50,
        V2_40 = (2 << 16) | 40,
        V2_31 = (2 << 16) | 31,
        V2_30 = (2 << 16) | 30,
        V2_20 = (2 << 16) | 20,
        V2_10 = (2 << 16) | 10,
        V2_00 =  2 << 16      ,
        V1_10 = (1 << 16) | 10,
        V1_02 = (1 << 16) |  2,
    }
}

impl Revision {
    pub const LATEST: Revision = Revision::V2_80;

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

c_enum! {
    pub enum Status(usize) {
        // Appendix D
        Success             = 0,

        WarnUnknownGlyph    = 1,
        WarnDeleteFailure   = 2,
        WarnWriteFailure    = 3,
        WarnBufferTooSmall  = 4,
        WarnStaleData       = 5,
        WarnFileSystem      = 6,
        WarnResetRequired   = 7,

        LoadError           = Status::ERROR_BIT | 1,
        InvalidParameter    = Status::ERROR_BIT | 2,
        Unsupported         = Status::ERROR_BIT | 3,
        BadBufferSize       = Status::ERROR_BIT | 4,
        BufferTooSmall      = Status::ERROR_BIT | 5,
        NotReady            = Status::ERROR_BIT | 6,
        DeviceError         = Status::ERROR_BIT | 7,
        WriteProtected      = Status::ERROR_BIT | 8,
        OutOfResources      = Status::ERROR_BIT | 9,
        VolumeCorrupted     = Status::ERROR_BIT | 10,
        VolumeFull          = Status::ERROR_BIT | 11,
        NoMedia             = Status::ERROR_BIT | 12,
        MediaChanged        = Status::ERROR_BIT | 13,
        NotFound            = Status::ERROR_BIT | 14,
        AccessDenied        = Status::ERROR_BIT | 15,
        NoResponse          = Status::ERROR_BIT | 16,
        NoMapping           = Status::ERROR_BIT | 17,
        Timeout             = Status::ERROR_BIT | 18,
        NotStarted          = Status::ERROR_BIT | 19,
        AlreadyStarted      = Status::ERROR_BIT | 20,
        Aborted             = Status::ERROR_BIT | 21,
        ICMPError           = Status::ERROR_BIT | 22,
        TFTPError           = Status::ERROR_BIT | 23,
        ProtocolError       = Status::ERROR_BIT | 24,
        IncompatibleVersion = Status::ERROR_BIT | 25,
        SecurityViolation   = Status::ERROR_BIT | 26,
        CRCError            = Status::ERROR_BIT | 27,
        EndOfMedia          = Status::ERROR_BIT | 28,
        EndOfFile           = Status::ERROR_BIT | 31,
        InvalidLanguage     = Status::ERROR_BIT | 32,
        CompromisedData     = Status::ERROR_BIT | 33,
        HTTPError           = Status::ERROR_BIT | 35,
    }
}

impl Status {
    pub const ERROR_BIT: usize = 0x1_usize.reverse_bits(); // high bit

    pub fn is_error(self) -> bool {
        (self.0 & Status::ERROR_BIT) != 0
    }

    pub fn is_warning(self) -> bool {
        self != Status::Success && !self.is_error()
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
        assert_eq!(status, Status::WarnUnknownGlyph);
        assert_ne!(status, Status::Success);
        assert_ne!(status, Status::LoadError);
    }

    #[test]
    #[allow(clippy::bool_assert_comparison)]
    #[allow(clippy::branches_sharing_code)]
    fn test_is_error() {
        assert_eq!(false, Status::Success.is_error());

        assert_eq!(false, Status::WarnUnknownGlyph.is_error());
        assert_eq!(false, Status::WarnFileSystem.is_error());
        assert_eq!(false, Status::WarnResetRequired.is_error());

        assert_eq!(true, Status::LoadError.is_error());
        assert_eq!(true, Status::AccessDenied.is_error());
        assert_eq!(true, Status::HTTPError.is_error());

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
    #[allow(clippy::bool_assert_comparison)]
    #[allow(clippy::branches_sharing_code)]
    fn test_is_warning() {
        assert_eq!(false, Status::Success.is_warning());

        assert_eq!(true, Status::WarnUnknownGlyph.is_warning());
        assert_eq!(true, Status::WarnFileSystem.is_warning());
        assert_eq!(true, Status::WarnResetRequired.is_warning());

        assert_eq!(false, Status::LoadError.is_warning());
        assert_eq!(false, Status::AccessDenied.is_warning());
        assert_eq!(false, Status::HTTPError.is_warning());

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
        assert_eq!(Status::Success.into_result(), Ok(Status::Success));
        assert_eq!(Status::WarnFileSystem.into_result(), Ok(Status::WarnFileSystem));
        assert_eq!(Status::LoadError.into_result(), Err(Status::LoadError));
        assert_eq!(Status::HTTPError.into_result(), Err(Status::HTTPError));
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
        );
    }

    fn verify_revision(&self) {
        let actual_revision = self.header().revision;
        assert!(
            actual_revision >= Self::MIN_REVISION,
            "Revision {} older than minimum supported revision {}",
            actual_revision,
            Self::MIN_REVISION,
        );
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
        );
    }

    fn verify_crc32(&self) {
        const CRC_FIELD_LENGTH: usize = size_of::<u32>();

        let header = self.header();
        let size = header.header_size as usize;
        let start_address = core::ptr::from_ref(self).cast::<u8>();
        let crc_field_address = core::ptr::addr_of!(header.crc32).cast::<u8>();

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
            "Calculated CRC {actual_remainder:x} does not match listed value \
             {orig_remainder:x}"
        );
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

impl SystemTable<'_> {
    /// Safe(r) wrapper around [`BootServices::exit_boot_services`] that fetches the
    /// latest memory map and ensures that pointers to boot services are removed from the
    /// system table on success.
    ///
    /// # Safety
    /// After this succeeds, pointers to functions that provide any kind of boot services
    /// are no longer valid. This includes the console streams and memory allocation,
    /// which may be referenced by globals outside of the control of this object. It is
    /// the caller's responsibility to make sure any dangling references are cleared or
    /// unused.
    pub unsafe fn exit_boot_services(&mut self, image_handle: Handle) -> MemoryMap {
        #![allow(clippy::missing_panics_doc)]
        let boot_services = self.boot_services.unwrap();
        let memory_map = boot_services.get_memory_map();
        // This shouldn't fail since we just got the memory map
        boot_services.exit_boot_services(image_handle, memory_map.key).unwrap();

        // Clear pointers to boot services and console streams, which are now invalid
        self.boot_services = None;
        self.console_in_handle = Handle::NULL;
        self.console_in = None;
        self.console_out_handle = Handle::NULL;
        self.console_out = None;
        self.std_err_handle = Handle::NULL;
        self.std_err = None;

        memory_map
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
    allocate_pages_: unsafe extern "C" fn(
        allocate_type: AllocateType,
        memory_type: MemoryType,
        page_count: usize,
        physical_address: *mut u64,
    ) -> Status,
    pub free_pages:
        unsafe extern "C" fn(physical_address: u64, page_count: usize) -> Status,
    get_memory_map_: unsafe extern "C" fn(
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
    exit_boot_services_:
        unsafe extern "C" fn(image_handle: Handle, memory_map_key: usize) -> Status,

    // Miscellaneous Services
    get_next_monotonic_count: usize,
    stall: usize,
    set_watchdog_timer: usize,

    // Driver Support Services
    connect_controller: usize,
    disconnect_controller: usize,

    // Open and Close Protocol Services
    open_protocol_: unsafe extern "C" fn(
        handle: Handle,
        guid: &GUID,
        interface: *mut *const c_void,
        agent_handle: Handle,
        controller_handle: Handle,
        attributes: OpenProtocolAttributes,
    ) -> Status,
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

impl BootServices {
    /// Allocate a number of pages of a given type of memory, optionally constraining its
    /// location.
    ///
    /// Pages are 4KB ([`PAGE_SIZE`]) on all platforms.
    ///
    /// The meaning of `reference_address` depends on `allocate_type`:
    ///   * [`AnyAddress`](AllocateType::AnyAddress): Search for a block of pages anywhere
    ///     in memory. `reference_address` is ignored, and should be `None`.
    ///   * [`MaxAddress`](AllocateType::MaxAddress): Search for a block of pages below
    ///     the given address.
    ///   * [`ExactAddress`](AllocateType::ExactAddress): Reserve the block of pages
    ///     starting at `reference_address`.
    ///
    /// # Errors
    /// This function will return:
    ///   * [`Status::OutOfResources`] if there was not enough memory available with the
    ///     specified constraints.
    ///   * [`Status::InvalidParameter`] if the memory type is unsupported.
    ///   * [`Status::NotFound`] if the requested memory location is out of bounds of
    ///     physical memory.
    pub fn allocate_pages(
        &self,
        allocate_type: AllocateType,
        memory_type: MemoryType,
        page_count: usize,
        reference_address: Option<u64>,
    ) -> core::result::Result<u64, Status> {
        let mut physical_address = reference_address.unwrap_or_default();
        unsafe {
            (self.allocate_pages_)(
                allocate_type,
                memory_type,
                page_count,
                &mut physical_address,
            )
            .into_result()?;
        }
        Ok(physical_address)
    }


    /// Get a map representing the status of all available memory.
    ///
    /// # Panics
    /// Panics if the firmware does not behave according to the spec.
    pub fn get_memory_map(&self) -> MemoryMap {
        let mut memory_map_size = 0_usize;
        let mut memory_map = MemoryMap::new();

        loop {
            memory_map.raw_map.resize(memory_map_size, 0);

            let result = unsafe {
                (self.get_memory_map_)(
                    &mut memory_map_size,
                    // TODO: Make sure this is aligned properly. memory_map.verify() will
                    // check and panic if it isn't, but we should be able to ensure it.
                    memory_map.raw_map.as_mut_ptr().cast(),
                    &mut memory_map.key,
                    &mut memory_map.descriptor_size,
                    &mut memory_map.descriptor_version,
                )
                .into_result()
            };
            match result {
                Ok(_) => break,
                Err(Status::BufferTooSmall) => {
                    // Allow room for another entry since we have to reallocate the buffer
                    memory_map_size += memory_map.descriptor_size;
                }
                // We shouldn't run into any of the other errors listed in the spec.
                Err(e) => panic!("Unexpected error from get_memory_map: {:?}", e),
            }
        }

        // Trim anything that wasn't used
        memory_map.raw_map.resize(memory_map_size, 0);

        memory_map.verify();
        memory_map
    }


    /// Get the implementation of a protocol offered by the given `handle`.
    ///
    /// For UEFI applications, `agent_handle` is the application's image handle. This
    /// method does not offer all the options required by UEFI drivers.
    ///
    /// # Errors
    /// Fails if `handle` is not valid, or it does not implement the specified protocol.
    ///
    /// # Panics
    /// Panics if the firmware does not behave according to the spec.
    pub fn get_protocol<T: proto::Protocol>(
        &self,
        handle: Handle,
        agent_handle: Handle,
    ) -> core::result::Result<&T, Status> {
        let mut protocol = core::ptr::null::<T>();
        unsafe {
            (self.open_protocol_)(
                handle,
                &T::PROTOCOL_ID,
                core::ptr::addr_of_mut!(protocol).cast(),
                agent_handle,
                Handle::NULL,
                OpenProtocolAttributes::Get,
            )
            .into_result()?;
            Ok(protocol.as_ref().unwrap())
        }
    }

    /// Signal to UEFI that the OS is now taking over.
    ///
    /// If this function exits successfully, the OS is now in charge of memory management,
    /// and it is no longer safe to call any functions on [`BootServices`].
    ///
    /// In order to ensure that the OS has an accurate picture of the system, the caller
    /// must pass the [`key`](MemoryMap::key) from a prior call to
    /// [`BootServices::get_memory_map`]. If it does not match the latest value, this
    /// function returns with an error and the caller will have to fetch a new map before
    /// trying again.
    ///
    /// # Safety
    /// If this function exits successfully, then this object is no longer valid. The
    /// pointer to this table should be removed from the [`SystemTable`] and any other
    /// copies should be deleted.
    ///
    /// If the function exits with an error, then it is only safe to call memory services
    /// like [`allocate_pages`](Self::allocate_pages) and
    /// [`get_memory_map`](Self::get_memory_map). Any other boot services may have been
    /// unloaded already.
    ///
    /// # Errors
    /// Will fail with [`Status::InvalidParameter`] if the `memory_map_key` does not match
    /// the latest value.
    pub unsafe fn exit_boot_services(
        &self,
        image_handle: Handle,
        memory_map_key: usize,
    ) -> Result {
        (self.exit_boot_services_)(image_handle, memory_map_key).into_result()
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

    #[allow(clippy::missing_panics_doc)]
    pub fn verify_version(&self) {
        assert!(
            self.descriptor_version >= Self::MIN_VERSION,
            "Descriptor version {} less than required version {}",
            self.descriptor_version,
            Self::MIN_VERSION,
        );
    }

    #[allow(clippy::missing_panics_doc)]
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

    #[allow(clippy::missing_panics_doc)]
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
    pub fn iter(&self) -> impl Iterator<Item = &MemoryDescriptor> {
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

c_enum! {
    pub enum MemoryType(u32) {
        /// Memory that is never available for use
        Reserved            = 0,
        /// Memory used for UEFI application code.
        LoaderCode          = 1,
        /// Memory allocated by UEFI applications.
        LoaderData          = 2,
        /// Memory used for drivers that provide [`BootServices`].
        BootServicesCode    = 3,
        /// Memory allocated by drivers that provide [`BootServices`].
        BootServicesData    = 4,
        /// Memory used for drivers that provide [`RuntimeServices`].
        RuntimeServicesCode = 5,
        /// Memory allocated by drivers that provide [`RuntimeServices`].
        RuntimeServicesData = 6,
        /// Free memory.
        Conventional        = 7,
        /// Damaged memory modules.
        Unusable            = 8,
        /// Memory that can be used after the OS initializes ACPI.
        ACPIReclaim         = 9,
        /// Memory that must be preserved in ACPI states S1–S3.
        ACPINonVolatile     = 10,
        /// Memory mapped to device I/O.
        MappedIO            = 11,
        /// Memory mapped to I/O ports.
        MappedIOPortSpace   = 12,
        /// Memory used by processor firmware code.
        ProcessorCode       = 13,
        /// Free nonvolatile memory.
        Persistent          = 14,

        /// Beginning of range (inclusive) for OEM-specific memory types
        MinOEMDefined       = 0x7000_0000,
        /// End of range (inclusive) for OEM-specific memory types
        MaxOEMDefined       = 0x7fff_ffff,
        /// Beginning of range (inclusive) for operating system-specific memory types
        MinOSDefined        = 0x8000_0000,
        /// End of range (inclusive) for operating system-specific memory types
        MaxOSDefined        = 0xffff_ffff,
    }
}

c_enum! {
    pub enum AllocateType(u32) {
        AnyAddress   = 0,
        MaxAddress   = 1,
        ExactAddress = 2,
    }
}

bitfield! {
    pub struct MemoryAttributes(u64) {
        [0] pub supports_uncacheable,
        [1] pub supports_write_combining,
        [2] pub supports_write_through,
        [4] pub supports_write_back,
        [5] pub supports_uncacheable_sem,
        [12] pub supports_write_protect,
        [13] pub supports_read_protect,
        [14] pub supports_exec_protect,
        [15] pub nonvolatile,
        [16] pub more_reliable,
        [17] pub supports_read_only,
        [18] pub specific_purpose,
        [19] pub supports_cpu_crypto,
        [63] pub runtime,
    }
}


c_enum! {
    pub enum OpenProtocolAttributes(u32) {
        ByHandle  = 1 << 0,
        Get       = 1 << 1,
        Test      = 1 << 2,
        ByChild   = 1 << 3,
        ByDriver  = 1 << 4,
        Exclusive = 1 << 5,
    }
}


pub mod proto {
    use super::{Handle, MemoryType, Result, Status, SystemTable, GUID};
    use core::ffi::c_void;
    use tartan_bitfield::bitfield;
    use tartan_c_enum::c_enum;

    pub trait Protocol {
        const PROTOCOL_ID: GUID;
    }


    #[repr(C)]
    #[derive(Debug)]
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
    #[derive(Debug, Copy, Clone, PartialEq, Eq)]
    pub struct SimpleTextOutputMode {
        pub max_mode: i32,
        pub mode: i32,
        pub attribute: i32,
        pub cursor_column: i32,
        pub cursor_row: i32,
        pub cursor_visible: bool,
    }


    #[repr(C)]
    #[derive(Debug)]
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
        pub const MIN_REVISION: u32 = 0x1000;
    }


    /// Protocol for accessing a file system supported by UEFI (typically FAT).
    #[repr(C)]
    pub struct SimpleFileSystem {
        /// Implemented revision of the `SimpleFileSystem` protocol.
        pub revision: u64,
        open_volume_: unsafe extern "C" fn(
            this: &SimpleFileSystem,
            root: &mut *const File,
        ) -> Status,
    }

    impl Protocol for SimpleFileSystem {
        const PROTOCOL_ID: GUID = GUID::from(0x964e5b22_6459_11d2_8e39_00a0c969723b);
    }

    impl SimpleFileSystem {
        /// Minimum supported [`SimpleFileSystem::revision`]. Future versions are
        /// guaranteed to be backwards-compatible.
        pub const MIN_REVISION: u32 = 0x0001_0000;

        /// Get a handle to the root of the file system.
        ///
        /// # Errors
        /// This method can fail for many reasons, including standard I/O issues like
        /// device errors or resource exhaustion. It will also fail if the file system on
        /// the device is not supported by the UEFI implementation.
        ///
        /// # Panics
        /// Panics if the firmware does not behave according to the spec.
        pub fn open_volume(&self) -> core::result::Result<&File, Status> {
            let mut root = core::ptr::null::<File>();
            unsafe {
                (self.open_volume_)(self, &mut root).into_result()?;
                Ok(root.as_ref().unwrap())
            }
        }
    }


    /// Protocol for accessing a file or directory.
    #[repr(C)]
    pub struct File {
        /// Implemented revision of the File protocol.
        pub revision: u64,

        open_: unsafe extern "C" fn(
            this: &File,
            file: &mut *const File,
            path: *const u16,
            mode: FileMode,
            attributes: FileAttributes,
        ) -> Status,
        close_: unsafe extern "C" fn(this: &File) -> Status,
        delete_: unsafe extern "C" fn(this: &File) -> Status,
        read_: unsafe extern "C" fn(
            this: &File,
            count: &mut usize,
            buffer: *mut c_void,
        ) -> Status,
        write_: unsafe extern "C" fn(
            this: &File,
            count: &mut usize,
            buffer: *const c_void,
        ) -> Status,
        get_position_: unsafe extern "C" fn(this: &File, position: &mut u64) -> Status,
        set_position_: unsafe extern "C" fn(this: &File, position: u64) -> Status,
        get_info: usize,
        set_info: usize,
        flush_: unsafe extern "C" fn(this: &File) -> Status,

        // Only available if revision >= 0x0002_0000
        open_v2: usize,
        read_v2: usize,
        write_v2: usize,
        flush_v2: usize,
    }

    impl File {
        /// Minimum supported [`SimpleFileSystem::revision`]. Future versions are
        /// guaranteed to be backwards-compatible.
        pub const MIN_REVISION: u32 = 0x0001_0000;

        /// Get a handle to a new file, relative to the directory represented by the
        /// current instance.
        ///
        /// `path` uses the Windows path format without a drive name, e.g. `\FOO\BAR.TXT`
        /// or `..\QUUX.DAT`.
        ///
        /// `attributes` is only used if the file is created.
        ///
        /// # Errors
        /// This method can fail for many reasons, including standard I/O issues like
        /// device errors or resource exhaustion. It can also fail if access is denied or
        /// there was an attempt to write to read-only media.
        ///
        /// # Panics
        /// Panics if `path` is empty or does not end in a null character.
        pub fn open(
            &self,
            path: &[u16],
            mode: FileMode,
            attributes: FileAttributes,
        ) -> core::result::Result<&File, Status> {
            assert!(!path.is_empty(), "Path cannot be empty");
            assert!(*path.last().unwrap() == 0, "Path must be null-terminated");

            let mut file = core::ptr::null::<File>();
            unsafe {
                (self.open_)(self, &mut file, path.as_ptr(), mode, attributes)
                    .into_result()?;
                Ok(file.as_ref().unwrap())
            }
        }

        /// Flush and close the file or directory represented by the current instance.
        ///
        /// # Panics
        /// Panics if the firmware does not behave according to the spec.
        pub fn close(&self) {
            unsafe {
                // The UEFI spec says this cannot fail
                assert!((self.close_)(self) == Status::Success);
            }
        }

        /// Delete the file or directory represented by the current instance.
        ///
        /// # Errors
        /// This method cannot fail with an error, but it will return
        /// [`Status::WarnDeleteFailure`] if the file could not be deleted.
        pub fn delete(&self) -> Status {
            unsafe { (self.delete_)(self) }
        }

        /// Read file contents or a directory entry into the buffer.
        ///
        /// If this is a file, it will read up to `buffer.len()` bytes. If it is a
        /// directory, it will read a single directory entry if that entry can fit in the
        /// buffer.
        ///
        /// On success, returns the number of bytes actually read.
        ///
        /// # Errors
        /// This method can fail for many reasons, including standard I/O issues like
        /// device errors or resource exhaustion. In addition, it will return:
        ///   * [`Status::BufferTooSmall`] if this is a directory and the next entry could
        ///     not fit into the buffer.
        ///   * [`Status::DeviceError`] if the current position was already EOF.
        pub fn read(&self, buffer: &mut [u8]) -> core::result::Result<usize, Status> {
            let mut count = buffer.len();
            unsafe {
                (self.read_)(self, &mut count, buffer.as_mut_ptr().cast())
                    .into_result()?;
            }
            Ok(count)
        }

        /// Write the contents of `buffer` out to the current position.
        ///
        /// On success, returns the number of bytes actually written, which will always
        /// be the full buffer.
        ///
        /// # Errors
        /// This method can fail for many reasons, including standard I/O issues like
        /// device errors or resource exhaustion. It addition, it will return:
        ///    * `Status::Unsupported` if this is a directory.
        ///    * `Status::AccessDenied` if the file is in read-only mode.
        pub fn write(&self, buffer: &[u8]) -> core::result::Result<usize, Status> {
            let mut count = buffer.len();
            unsafe {
                (self.write_)(self, &mut count, buffer.as_ptr().cast()).into_result()?;
            }
            Ok(count)
        }

        /// Get the handle's current position in the file.
        ///
        /// # Errors
        /// This function will fail with:
        ///    * [`Status::DeviceError`] if the file has been deleted.
        ///    * [`Status::Unsupported`] if this is a directory.
        pub fn get_position(&self) -> core::result::Result<u64, Status> {
            let mut position = 0_u64;
            unsafe {
                (self.get_position_)(self, &mut position).into_result()?;
            }
            Ok(position)
        }

        /// Set the handle's current position in the file.
        ///
        /// If the position is `u64::MAX`, this will seek to the end of the file.
        /// Otherwise, it seeks to the absolute position in bytes. If the position is
        /// greater than the current file size, the file will grow to the given size.
        ///
        /// # Errors
        /// This function will fail with:
        ///    * [`Status::DeviceError`] if the file has been deleted.
        ///    * [`Status::Unsupported`] if this is a directory.
        pub fn set_position(&self, position: u64) -> Result {
            unsafe { (self.set_position_)(self, position).into_result() }
        }

        /// Write any buffered changes.
        ///
        /// # Errors
        /// This method can fail for many reasons, including standard I/O issues like
        /// device errors or resource exhaustion. It can also fail if access is denied or
        /// there was an attempt to write to read-only media.
        pub fn flush(&self) -> Result {
            unsafe { (self.flush_)(self).into_result() }
        }
    }


    c_enum! {
        /// Controls how a file is opened.
        ///
        /// The UEFI spec defines separate read/write/create flags, but they are only
        /// valid in specific combinations, so this enum defines those combinations.
        pub enum FileMode(u64) {
            /// File will be read, and should exist already.
            Read = 0x1,
            /// File will be read and written, and should exist already.
            ReadWrite = 0x3,
            /// File will be read and written, and will be created if it does not exist.
            ReadWriteCreate = 0x8000_0000_0000_0003,
        }
    }

    bitfield! {
        /// Basic properties of a file or directory as defined by the FAT format.
        pub struct FileAttributes(u64) {
            /// Indicates the file should not be written.
            [0] read_only,
            /// Indicates the file should not be visible under normal circumstances.
            [1] hidden,
            /// Indicates the file is important to system operation.
            [2] system,
            /// Indicates that this is a folder.
            [4] directory,
            /// Indicates the file should needs to be backed up.
            [5] archive,
        }
    }
}
