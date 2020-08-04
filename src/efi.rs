// See EDK MdePkg/Include/Uefi/UefiSpec.h

#![allow(unused)]

#[repr(transparent)]
#[derive(PartialEq, Eq)]
pub struct Handle(usize);

#[repr(transparent)]
#[derive(PartialEq, Eq)]
pub struct Revision(pub u32);

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

pub type Result = core::result::Result<Status, Status>;

#[repr(transparent)]
#[derive(Debug, PartialEq, Eq)]
pub struct Status(usize);

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

    pub fn is_error(&self) -> bool {
        (self.0 & Status::ERROR_BIT) != 0
    }

    pub fn is_warning(&self) -> bool {
        *self != Status::SUCCESS && !self.is_error()
}

    pub fn into_result(self) -> Result {
        if self.is_error() {
            Err(self)
}
        else {
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

        if cfg!(target_pointer_width="32") {
            assert_eq!(Status(0x0000_0000).is_error(), false);
            assert_eq!(Status(0x0000_0001).is_error(), false);
            assert_eq!(Status(0x7fff_ffff).is_error(), false);
            assert_eq!(Status(0x8000_0000).is_error(), true);
            assert_eq!(Status(0xffff_ffff).is_error(), true);
        }
        else {
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

        if cfg!(target_pointer_width="32") {
            assert_eq!(Status(0x0000_0000).is_warning(), false);
            assert_eq!(Status(0x0000_0001).is_warning(), true);
            assert_eq!(Status(0x7fff_ffff).is_warning(), true);
            assert_eq!(Status(0x8000_0000).is_warning(), false);
            assert_eq!(Status(0xffff_ffff).is_warning(), false);
        }
        else {
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

#[repr(C)]
pub struct TableHeader {
    pub signature: u64,
    pub revision: Revision,
    pub header_size: u32,
    pub crc32: u32,
    reserved: u32,
}

#[repr(C)]
pub struct SystemTable {
    pub header: TableHeader,
    pub firmware_vendor: *const u16,
    pub firmware_revision: u32,
    pub console_in_handle: Handle,
    pub console_in: *const proto::SimpleTextInput,
    pub console_out_handle: Handle,
    pub console_out: *const proto::SimpleTextOutput,
    pub std_err_handle: Handle,
    pub std_err: proto::SimpleTextOutput,
    pub runtime_services: RuntimeServices,
    pub boot_services: BootServices,
    pub config_entry_count: usize,
    pub config_table: *const ConfigurationTable,
}

impl SystemTable {
    pub const SIGNATURE: u64 = 0x5453_5953_2049_4249;
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

impl RuntimeServices {
    pub const SIGNATURE: u64 = 0x5652_4553_544e_5552;
}

#[repr(C)]
pub struct BootServices {
    pub header: TableHeader,

    // Task Priority Services
    raise_tpm: usize,
    restore_tpl: usize,

    // Memory Services
    allocate_pages: usize,
    free_pages: usize,
    get_memory_map: usize,
    allocate_pool: usize,
    free_pool: usize,

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
    handle_protocol: usize,
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

impl BootServices {
    pub const SIGNATURE: u64 = 0x5652_4553_544f_4f42;
}

#[repr(C)]
#[derive(Debug, PartialEq, Eq)]
pub struct GUID(
    u32,
    u16,
    u16,
    [u8; 8]
);

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
    fn from(n: u128) -> Self { GUID::from(n) }
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
        assert_eq!(guid.3, [
            0x09_u8, 0x10_u8, 0x11_u8, 0x12_u8,
            0x13_u8, 0x14_u8, 0x15_u8, 0x16_u8,
        ]);
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

impl ConfigurationTable {
    pub const ACPI_20_GUID:    GUID = GUID::from(0x8868e871_e4f1_11d3_bc22_0080c73c8881);
    pub const ACPI_GUID:       GUID = GUID::from(0xeb9d2d30_2d88_11d3_9a16_0090273fc14d);
    pub const SAL_SYSTEM_GUID: GUID = GUID::from(0xeb9d2d32_2d88_11d3_9a16_0090273fc14d);
    pub const SMBIOS_GUID:     GUID = GUID::from(0xeb9d2d31_2d88_11d3_9a16_0090273fc14d);
    pub const SMBIOS3_GUID:    GUID = GUID::from(0xf2fd1544_9794_4a2c_992e_e5bbcf20e394);
    pub const MPS_GUID:        GUID = GUID::from(0xeb9d2d2f_2d88_11d3_9a16_0090273fc14d);
    // TODO: ... more defined in sect. 4.6
}

pub mod proto {
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
        pub reset: extern "C" fn(this: &SimpleTextOutput, extended_verification: bool) -> Status,
        pub output_string: extern "C" fn(this: &SimpleTextOutput, string: *const u16) -> Status,
        pub test_string: extern "C" fn(this: &SimpleTextOutput, string: *const u16) -> Status,
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
}
