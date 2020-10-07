//! Support for Executable and Linkable Format (ELF) binaries.

#![no_std]
#![warn(missing_docs)]
#![warn(clippy::pedantic)]

use core::mem::size_of;
use static_assertions::const_assert_eq;
use tartan_bitfield::{bitfield, Bitfield};
use tartan_c_enum::c_enum;


/// Variant of the [`Header`] structure native to the current target.
pub type HeaderNative = Header<usize>;

/// File header for an ELF file with a specific address size.
#[repr(C)]
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct Header<Addr: Copy> {
    /// Header common to all ELF variants.
    pub ident: HeaderIdent,
    /// Distinguishes executables, libraries, etc.
    pub file_type: FileType,
    /// CPU architecture
    pub machine: Machine,
    /// ELF format version
    pub version: u32,
    /// Virtual address of executable entry point. Zero if not present.
    pub entry_point: Addr,
    /// File offset to the array of program section headers which control executable
    /// loading. Zero if not present.
    pub program_header_offset: Addr,
    /// File offset to the array of section header which control linking. Zero if not
    /// present.
    pub section_header_offset: Addr,
    /// CPU architecture-specific flags.
    pub flags: u32,
    /// Size in bytes of this header. May be longer than the structure in code.
    pub header_size: u16,
    /// Size in bytes of of each program segment header. May be longer than the structure
    /// in code.
    pub program_header_size: u16,
    /// Number of program segment headers.
    pub program_header_count: u16,
    /// Size in bytes of each section header. May be longer than the structure in code.
    pub section_header_size: u16,
    /// Number of section headers.
    pub section_header_count: u16,
    /// Index of the section header which contains the list of section names.
    pub section_names_index: u16,
}

const_assert_eq!(52, size_of::<Header<u32>>());
const_assert_eq!(64, size_of::<Header<u64>>());
#[cfg(target_pointer_width = "32")]
const_assert_eq!(size_of::<HeaderNative>(), size_of::<Header<u32>>());
#[cfg(target_pointer_width = "64")]
const_assert_eq!(size_of::<HeaderNative>(), size_of::<Header<u64>>());


/// Initial header which has the same layout in all ELF variants and determines how to
/// interpret the rest of the file (endianness, sizes).
#[repr(C)]
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct HeaderIdent {
    /// Magic number that identifies this as an ELF file.
    pub magic: [u8; 4],
    /// Specifies the address size (32- or 64-bit) of the file.
    pub class: FileClass,
    /// Specifies whether the file uses big or little endian byte ordering.
    pub endianness: Endianness,
    /// Version of the ELF header.
    pub header_version: u8,
    /// Operating-system-specific ABI extensions that this file uses.
    pub os_abi: OSABI,
    /// Backwards-incompatible version number of the [`os_abi`](Self::os_abi) extensions
    /// that this file uses.
    pub os_abi_version: u8,
    _pad: [u8; 7],
}

const_assert_eq!(16, size_of::<HeaderIdent>());

impl HeaderIdent {
    /// Expected value of the [`magic`](Self::magic) field.
    pub const MAGIC: [u8; 4] = [0x7f, 0x45, 0x4c, 0x46];

    /// Expected value of the [`header_version`](Self::header_version) field.
    pub const VERSION: u8 = 1;
}


c_enum! {
    /// Specifies the address size (32- or 64-bit) of the file.
    pub enum FileClass(u8) {
        /// Invalid placeholder value.
        None,
        /// File uses 32-bit addresses/offsets.
        Size32,
        /// File uses 64-bit addresses/offsets.
        Size64,
    }
}


c_enum! {
    /// Specifies whether the file uses big or little endian byte ordering.
    pub enum Endianness(u8) {
        /// Invalid placeholder value.
        None,
        /// File uses little-endian (LSB first) byte ordering.
        Little,
        /// File uses big-endian (MSB-first) byte ordering.
        Big,
    }
}


c_enum! {
    /// Operating-system-specific ABI extensions supported by the ELF specification.
    pub enum OSABI(u8) {
        /// Original Unix System V ABI
        None,
        /// HP-UX
        HPUX,
        /// NetBSD
        NetBSD,
        /// GNU with Linux kernel
        Linux,
        /// GNU with Hurd kernel
        Hurd,
        /// Sun/Oracle Solaris
        Solaris = 6,
        /// IBM AIX
        AIX,
        /// SGI IRIX
        IRIX,
        /// FreeBSD
        FreeBSD,
        /// Compaq TRU64 Unix
        TRU64,
        /// Novell Modesto
        Modesto,
        /// OpenBSD
        OpenBSD,
        /// DEC OpenVMS
        OpenVMS,
        /// HP Non-Stop Kernel
        NonStopKernel,
        /// AROS (Amiga)
        AROS,
        /// FenixOS (Passas and Karlsson 2011?)
        FenixOS,
        /// Nuxi CloudABI
        CloudABI,
        /// Stratus Open VOS
        OpenVOS,

        /// Beginning of range (inclusive) for CPU architecture-specific values
        MinArchDefined = 0x40,
        /// End of range (inclusive) for CPU architecture-specific values
        MaxArchDefined = 0xff,
    }
}


c_enum! {
    /// Types of object files supported by the ELF specification
    pub enum FileType(u16) {
        /// Invalid placeholder value.
        None,
        /// Relocatable object file
        Relocatable,
        /// A file with an entry point that can be executed
        Executable,
        /// Dynamic shared library
        SharedObject,
        /// Core dump
        Core,
        /// Beginning of range (inclusive) for OS-specific values
        MinOSDefined = 0xfe00,
        /// End of range (inclusive) for OS-specific values
        MaxOSDefined = 0xfeff,
        /// Beginning of range (inclusive) for CPU architecture-specific values
        MinArchDefined = 0xff00,
        /// Beginning of range (inclusive) for CPU architecture-specific values
        MaxArchDefined = 0xffff,
    }
}


c_enum! {
    /// Identifies the target CPU architecture.
    ///
    /// The variants defined here are a small subset of the values supported by the System
    /// V ABI. There are lots of obscure platforms it doesn't make sense to list.
    pub enum Machine(u8) {
        /// No specific architecture defined.
        None,
        /// Sun/Oracle SPARC
        SPARC = 2,
        /// 32-bit x86
        X86 = 3,
        /// Motorola 68000
        M68K = 4,
        /// MIPS I
        MIPS = 8,
        /// MIPS RS3000 (little-endian)
        MIPSRS3kLittleEndian = 10,
        /// 32-bit PowerPC
        PowerPC = 20,
        /// 64-bit PowerPC
        PowerPC64 = 21,
        /// 32-bit Arm
        Arm = 40,
        /// Intel Itanium IA-64
        IA64 = 50,
        /// 64-bit x86
        X86_64 = 62,
        /// 64-bit Arm
        Arm64 = 183,
        /// RISC-V
        RISCV = 243,
    }
}


/// Program header variant native to the current target
#[cfg(target_pointer_width = "32")]
pub type ProgramHeaderNative = ProgramHeader32;

/// Program header variant native to the current target
#[cfg(target_pointer_width = "64")]
pub type ProgramHeaderNative = ProgramHeader64;


/// Header that controls loading of a single segment of an executable (32-bit variant)
#[repr(C)]
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct ProgramHeader32 {
    /// Indicates the contents/meaning of the segment described by this header.
    pub segment_type: ProgramSegmentType,
    /// File offset to the start of this segment's data.
    pub file_offset: u32,
    /// Virtual base address where the segment should be loaded.
    pub virtual_addr: u32,
    /// Physical base address where the segment should be loaded. Typically ignored.
    pub physical_addr: u32,
    /// Size in bytes of the file data for this segment.
    pub file_size: u32,
    /// Size in bytes of this segment once it is loaded into memory.
    pub mem_size: u32,
    /// Controls permissions and other OS/arch-specific flags.
    pub flags: ProgramSegmentFlags,
    /// Alignment of the segment in bytes, for both the file and memory.
    pub alignment: u32,
}

const_assert_eq!(32, size_of::<ProgramHeader32>());


/// Header that controls loading of a single segment of an executable (64-bit variant)
#[repr(C)]
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct ProgramHeader64 {
    /// Indicates the contents/meaning of the segment described by this header.
    pub segment_type: ProgramSegmentType,
    /// Controls permissions and other OS/arch-specific flags.
    pub flags: ProgramSegmentFlags,
    /// File offset to the start of this segment's data.
    pub file_offset: u64,
    /// Virtual base address where the segment should be loaded.
    pub virtual_addr: u64,
    /// Physical base address where the segment should be loaded. Typically ignored.
    pub physical_addr: u64,
    /// Size in bytes of the file data for this segment.
    pub file_size: u64,
    /// Size in bytes of this segment once it is loaded into memory.
    pub mem_size: u64,
    /// Alignment of the segment in bytes, for both the file and memory.
    pub alignment: u64,
}

const_assert_eq!(56, size_of::<ProgramHeader64>());


c_enum! {
    /// Defines the contents and meaning of individual program segments.
    pub enum ProgramSegmentType(u32) {
        /// Marks an ignored or unused segment.
        None,
        /// Segment that should be loaded into memory.
        Loadable,
        /// Segment containing information for the dynamic linker.
        DynamicLink,
        /// Segment containing the path to the program's interpreter (typically the
        /// dynamic loader).
        Interpreter,
        /// Segment containing auxillary information.
        Note,
        /// Segment that contains the program header itself, including all segment
        /// headers.
        ProgramHeaderTable = 6,
        /// Segment that contains a template for the thread-local storage area.
        ThreadLocalStorage,

        /// Beginning of range (inclusive) for OS-specific values
        MinOSDefined  = 0x6000_0000,
        /// End of range (inclusive) for OS-specific values
        MaxOSDefined  = 0x6FFF_FFFF,
        /// Beginning of range (inclusive) for CPU architecture-specific values
        MinArchDefined = 0x7000_0000,
        /// End of range (inclusive) for CPU architecture-specific values
        MaxArchDefined = 0x7FFF_FFFF,
    }
}


bitfield! {
    /// Permissions and other OS/arch-specific flags for individual program segments.
    pub struct ProgramSegmentFlags(u32) {
        /// Indicates the segment contains code that should be executable by the process.
        [0] pub execute,
        /// Indicates the segment contains data that should be writable by the process.
        [1] pub write,
        /// Indicates the segment contains data that should be readable by the process.
        [2] pub read,

        /// Bits that are left for OS-specific flags.
        [20..28] pub os_defined: u8,
        /// Bits that are left for CPU architecture-specific flags.
        [28..32] pub arch_defined: u8,
    }
}


/// Variant of the [`SectionHeader`] structure native to the current target
pub type SectionHeaderNative = SectionHeader<usize>;

/// Header that controls linking
#[repr(C)]
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct SectionHeader<Addr: Copy> {
    /// Offset within the names section to the start of this section's name.
    pub name_table_offset: u32,
    /// Indicates the contents/meaning of the section described by this header.
    pub section_type: SectionType,
    /// Flags for various attributes of this section.
    ///
    /// This is the raw numeric value of the flags. Use the [`flags`](Self::flags)
    /// accessor to view as a [`SectionFlags`] type.
    pub flags_raw: Addr,
    /// Virtual address where this section should be loaded in memory, if applicable.
    pub virtual_addr: Addr,
    /// File offset to the start of this section's data.
    pub file_offset: Addr,
    /// Size in bytes of this section's file data.
    pub file_size: Addr,
    /// Depending on the [`section_type`](Self::section_type), this may identify another
    /// related section.
    pub associated_section_index: u32,
    /// Additional flags that depend on the [`section_type`](Self::section_type).
    pub extra_info: u32,
    /// Required alignment of the section in bytes.
    pub alignment: Addr,
    /// Depending on the [`section_type`](Self::section_type), this may identify the size
    /// of individual data entries within the section.
    pub entry_size: Addr,
}

const_assert_eq!(40, size_of::<SectionHeader<u32>>());
const_assert_eq!(64, size_of::<SectionHeader<u64>>());
#[cfg(target_pointer_width = "32")]
const_assert_eq!(size_of::<SectionHeaderNative>(), size_of::<SectionHeader<u32>>());
#[cfg(target_pointer_width = "64")]
const_assert_eq!(size_of::<SectionHeaderNative>(), size_of::<SectionHeader<u64>>());

impl<Addr> SectionHeader<Addr>
where
    Addr: Copy + From<SectionFlags> + Into<SectionFlags>,
{
    /// Flags for various attributes of this section.
    pub fn flags(&self) -> SectionFlags {
        self.flags_raw.into()
    }

    /// Flags for various attributes of this section.
    pub fn set_flags(&mut self, flags: SectionFlags) {
        self.flags_raw = flags.into();
    }
}


c_enum! {
    /// Defines the contents and meaning of individual sections.
    pub enum SectionType(u32) {
        /// Marks an ignored or unused section.
        None,
        /// Section containing data that is only meaningful to the program.
        ProgramDefined,
        /// Section containing the symbol table for this object file.
        Symbols,
        /// Section containing packed null-terminated strings.
        Strings,
        /// Section containing relocation data with addends given in the section itself.
        RelocationWithAdded,
        /// Section containing the hash table for symbols.
        SymbolHash,
        /// Section containing information for the dynamic linker.
        DynamicLink,
        /// Section containing auxillary information.
        Note,
        /// An empty file section that will be filled with zeros when loaded in memory
        /// (BSS).
        NoBits,
        /// Section containing relocation data where the addends are determined by
        /// context in the target section.
        RelocationNoAdded,
        /// Section containing the symbol table for dynamic linking.
        DynamicLinkSymbol = 11,
        /// Section containing the addresses of constructor functions that should be run
        /// when the program is loaded.
        Constructors,
        /// Section containing the addresses of destructor functions that should be run
        /// when the program is unloaded.
        Destructors,
        /// Section containing the addresses of constructor functions that should be run
        /// before other [`Constructors`](Self::Constructors) when the program is loaded.
        PreConstructors,
        /// Section that groups other sections.
        Group,
        /// Table of section indexes that correspond to entries in the symbol table with
        /// indirect sections.
        SymbolIndirectSections,

        /// Beginning of range (inclusive) for OS-specific values
        MinOSDefined = 0x6000_0000,
        /// End of range (inclusive) for OS-specific values
        MaxOSDefined = 0x6fff_ffff,
        /// Beginning of range (inclusive) for CPU architecture-specific values
        MinArchDefined = 0x7000_0000,
        /// End of range (inclusive) for CPU architecture-specific values
        MaxArchDefined = 0x7fff_ffff,
        /// Beginning of range (inclusive) for user-defined values
        MinUserDefined = 0x8000_0000,
        /// End of range (inclusive) for user-defined values
        MaxUserDefined = 0xffff_ffff,
    }
}


bitfield! {
    /// Additional attributes for object file sections.
    pub struct SectionFlags(u32) {
        /// Indicates the section contains data that should be writable by the process.
        [ 0] pub write,
        /// Indicates this section should be loaded into memory when the process is run.
        [ 1] pub allocate,
        /// Indicates this section contains code that should be executable by the process.
        [ 2] pub execute,
        /// Indicates that multiple sections of this type can be combined while deleting
        /// duplicate contents.
        [ 4] pub merged,
        /// Indicates the section contains null-terminate strings.
        [ 5] pub strings,
        /// Indicates that the [`extra_info`](SectionHeader::extra_info) field of the
        /// section header contains an index to another section header.
        [ 6] pub extra_info_is_link,
        /// Indicates that this section and the [`associated_section_index`] should
        /// maintain their relative order when linking.
        [ 7] pub keep_order,
        /// Indicates that the section must be processed in an OS-defined way during
        /// linking.
        [ 8] pub os_nonconforming,
        /// Indicates the section is listed by a section with type [`SectionType::Group`].
        [ 9] pub group_member,
        /// Indicates the section contains a template for the thread-local storage area.
        [10] pub thread_locals,
        /// Bits that are left for OS-defined flags.
        [20..28] pub os_defined: u8,
        /// Bits that are left for CPU architecture-defined flags.
        [28..32] pub arch_defined: u8,
    }
}

impl From<usize> for SectionFlags {
    fn from(value: usize) -> SectionFlags {
        #![allow(clippy::cast_possible_truncation)]
        SectionFlags(value as u32)
    }
}

impl From<SectionFlags> for usize {
    fn from(flags: SectionFlags) -> usize {
        flags.value() as usize
    }
}
