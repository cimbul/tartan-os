//! Flattened device tree, a.k.a. devicetree blob (DTB), as defined in chapter 5 of the
//! Devicetree spec.

use crate::{MemoryReservation, Value};
use core::mem::{size_of, size_of_val};
use core::slice;
use core::str;
use nom::branch::alt;
use nom::bytes::complete as bytes;
use nom::combinator::{map, map_parser, value, verify};
use nom::error::{ErrorKind, ParseError};
use nom::number::complete as num;
use nom::sequence::{preceded, terminated};
use nom::IResult;
use tartan_parsers::error::{err, GeneralParseError};
use tartan_parsers::{opcode, result_iterator, struct_parser};


/// Represents a full flattened device tree blob (DTB).
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct Tree<'a> {
    header: Header,
    memory_reservation_block: &'a [u8],
    structure_block: &'a [u8],
    string_block: &'a [u8],
}

impl<'a> Tree<'a> {
    /// Parse a devicetree blob that begins at the specified memory address.
    ///
    /// # Safety
    /// The pointer must hold the address of the first byte of a valid devicetree blob,
    /// and it must uphold all the invariants of [`core::slice::from_raw_parts`] for a
    /// slice beginning at the specified address and extending the total length of the DTB
    /// as given in its header.
    ///
    /// # Errors
    /// If the header does not have the correct magic number, or any other properties of
    /// the header are invalid, this function will return an error.
    pub unsafe fn from_ptr<E: ParseError<&'a [u8]>>(
        ptr: *const u8,
    ) -> Result<Self, nom::Err<E>> {
        let header_data = slice::from_raw_parts(ptr, Header::SIZE);
        let (_, header) = Header::parse(header_data)?;

        let full_data = slice::from_raw_parts(ptr, header.total_size as usize);

        Ok(Self::from_buffer_and_header(full_data, header))
    }


    /// Parse a devicetree blob that starts at the beginning of the slice and extends no
    /// further than its end.
    ///
    /// # Errors
    /// If the header does not have the correct magic number, or any other properties of
    /// the header are invalid, this function will return an error.
    pub fn from_buffer<E: ParseError<&'a [u8]>>(
        data: &'a [u8],
    ) -> Result<Self, nom::Err<E>> {
        let (_, header) = Header::parse(data)?;
        Ok(Self::from_buffer_and_header(data, header))
    }


    fn from_buffer_and_header(data: &'a [u8], header: Header) -> Self {
        let mut start: usize;
        let mut len: usize;

        start = header.memory_reservation_block_offset.try_into().unwrap();
        let end = header.structure_block_offset.try_into().unwrap();
        let memory_reservation_block = &data[start..end];

        start = header.structure_block_offset.try_into().unwrap();
        len = header.structure_block_size.try_into().unwrap();
        let structure_block = &data[start..start + len];

        start = header.string_block_offset.try_into().unwrap();
        len = header.string_block_size.try_into().unwrap();
        let string_block = &data[start..start + len];

        Self { header, memory_reservation_block, structure_block, string_block }
    }


    /// Iterate over memory reservations defined by this devicetree
    pub fn memory_reservation_iter<E: ParseError<&'a [u8]> + 'a>(
        self: &'a Tree<'a>,
    ) -> impl Iterator<Item = Result<MemoryReservation, nom::Err<E>>> + 'a {
        result_iterator(self.memory_reservation_block, MemoryReservation::parse)
    }


    /// Iterate over elements in the device tree. Note that this does not directly iterate
    /// over *nodes*, but rather events that indicate the start of a node (including its
    /// children), a property defined on the node, or the end of a node.
    pub fn structure_iter<E: GeneralParseError<&'a [u8]>>(
        self: &'a Tree<'a>,
    ) -> impl Iterator<Item = Result<StructureData<'a>, nom::Err<E>>> {
        result_iterator(self.structure_block, StructureToken::parse)
            .take_while(|s| !matches!(s, Ok(StructureToken::End)))
            .filter_map(move |s| match s {
                Ok(StructureToken::BeginNode(n)) => Some(Ok(StructureData::BeginNode(n))),
                Ok(StructureToken::EndNode) => Some(Ok(StructureData::EndNode)),
                Ok(StructureToken::Property { name_offset, value }) => {
                    let value = Value { data: value };
                    match self.get_string(name_offset) {
                        Err(e) => Some(Err(e)),
                        Ok(name) => Some(Ok(StructureData::Property { name, value })),
                    }
                }
                Ok(StructureToken::NoOp | StructureToken::End) => None,
                Err(e) => Some(Err(e)),
            })
    }


    /// Retrieve a null-terminated string at the given offset in the strings block
    fn get_string<E: GeneralParseError<&'a [u8]>>(
        self: &Tree<'a>,
        offset: u32,
    ) -> Result<&'a str, nom::Err<E>> {
        let offset = offset as usize;
        let (_, string) = parse_c_string(&self.string_block[offset..])?;
        Ok(string)
    }
}


#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
struct Header {
    pub total_size: u32,
    pub structure_block_offset: u32,
    pub string_block_offset: u32,
    pub memory_reservation_block_offset: u32,
    pub version: u32,
    pub last_compatible_version: u32,
    pub boot_cpu_id: u32,
    pub string_block_size: u32,
    pub structure_block_size: u32,
}

impl Header {
    /// Expected size of serialized header
    pub const SIZE: usize = size_of_val(&Self::MAGIC) + size_of::<Self>();

    const MAGIC: [u8; 4] = [0xd0, 0x0d, 0xfe, 0xed];
    const VERSION: u32 = 17;

    pub fn parse<'a, E: ParseError<&'a [u8]>>(i: &'a [u8]) -> IResult<&'a [u8], Self, E> {
        preceded(
            bytes::tag(Self::MAGIC),
            verify(
                struct_parser! {
                    Self {
                        total_size: num::be_u32,
                        structure_block_offset: num::be_u32,
                        string_block_offset: num::be_u32,
                        memory_reservation_block_offset: num::be_u32,
                        version: num::be_u32,
                        last_compatible_version: num::be_u32,
                        boot_cpu_id: num::be_u32,
                        string_block_size: num::be_u32,
                        structure_block_size: num::be_u32,
                    }
                },
                |h| h.last_compatible_version <= Self::VERSION,
            ),
        )(i)
    }
}


impl MemoryReservation {
    fn parse<'a, E: ParseError<&'a [u8]>>(i: &'a [u8]) -> IResult<&'a [u8], Self, E> {
        let parser = struct_parser! {
            Self {
                address: num::be_u64,
                size: num::be_u64,
            }
        };
        parser(i)
    }
}


/// An event that signals the start/end of a node or a property of one while walking the
/// device tree.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StructureData<'a> {
    /// Indicates that the following events are within the scope of a new node defined
    /// as the child of the previous current node.
    BeginNode(&'a str),
    /// Indicates that the previous current node is complete, and should be popped off
    /// the stack and replaced with its parent.
    EndNode,
    /// Defines a property of the current node
    Property {
        /// The name of the property.
        ///
        /// The Devicetree spec lists a number of common property names, but the system
        /// is open for extension, so it may not be recognizable.
        name: &'a str,

        /// The value of the property.
        ///
        /// The format of this data depends on the property name. Types defined by the
        /// Devicetree spec include:
        ///   * empty values,
        ///   * null-terminated strings or lists of them,
        ///   * big-endian 32-bit or 64-bit integers
        ///   * "phandles," or 32-bit numbers that uniquely identify another node
        ///
        /// However, different properties can define their own representation, so it is
        /// not possible to infer the type of an unrecognized property.
        value: Value<'a>,
    },
}


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StructureToken<'a> {
    BeginNode(&'a str),
    EndNode,
    Property { name_offset: u32, value: &'a [u8] },
    NoOp,
    End,
}

impl<'a> StructureToken<'a> {
    fn parse<E: GeneralParseError<&'a [u8]>>(i: &'a [u8]) -> IResult<&'a [u8], Self, E> {
        alt((
            Self::parse_begin_node,
            Self::parse_end_node,
            Self::parse_property,
            Self::parse_no_op,
            Self::parse_end,
        ))(i)
    }

    fn parse_begin_node<E: GeneralParseError<&'a [u8]>>(
        i: &'a [u8],
    ) -> IResult<&'a [u8], Self, E> {
        opcode(
            "begin node",
            bytes::tag(1_u32.to_be_bytes()),
            map(alignment_padded(4, parse_c_string), Self::BeginNode),
        )(i)
    }

    fn parse_end_node<E: ParseError<&'a [u8]>>(
        i: &'a [u8],
    ) -> IResult<&'a [u8], Self, E> {
        value(Self::EndNode, bytes::tag(2_u32.to_be_bytes()))(i)
    }

    fn parse_property<E: GeneralParseError<&'a [u8]>>(
        i: &'a [u8],
    ) -> IResult<&'a [u8], Self, E> {
        opcode(
            "property",
            bytes::tag(3_u32.to_be_bytes()),
            alignment_padded(4, |i| {
                let (i, value_length) = num::be_u32(i)?;
                let (i, name_offset) = num::be_u32(i)?;
                let (i, value) = bytes::take(value_length)(i)?;
                Ok((i, Self::Property { name_offset, value }))
            }),
        )(i)
    }

    fn parse_no_op<E: ParseError<&'a [u8]>>(i: &'a [u8]) -> IResult<&'a [u8], Self, E> {
        value(Self::NoOp, bytes::tag(4_u32.to_be_bytes()))(i)
    }

    fn parse_end<E: ParseError<&'a [u8]>>(i: &'a [u8]) -> IResult<&'a [u8], Self, E> {
        value(Self::End, bytes::tag(9_u32.to_be_bytes()))(i)
    }
}


fn parse_c_string<'a, E: GeneralParseError<&'a [u8]>>(
    i: &'a [u8],
) -> IResult<&'a [u8], &'a str, E> {
    let null_terminated_bytes =
        terminated(bytes::take_until(&[0_u8] as &[u8]), bytes::tag([0_u8]));
    map_parser(null_terminated_bytes, |b| match str::from_utf8(b) {
        Ok(s) => Ok((&[] as &[u8], s)),
        Err(_) => err(i, ErrorKind::Verify),
    })(i)
}


fn alignment_padded<'a, P, O, E>(
    alignment_bytes: usize,
    parser: P,
) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], O, E>
where
    E: ParseError<&'a [u8]>,
    P: Fn(&'a [u8]) -> IResult<&'a [u8], O, E>,
{
    move |i| {
        let (i, o) = parser(i)?;

        // Skip any bytes necessary to keep the remaining input aligned
        let address = i.as_ptr() as usize;
        let remainder = address % alignment_bytes;
        let padding = if remainder == 0 { 0 } else { alignment_bytes - remainder };
        let (i, _) = bytes::take(padding)(i)?;

        Ok((i, o))
    }
}
