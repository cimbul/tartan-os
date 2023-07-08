//! Parsers for AML bytecode.
//!
//! AML bytecode grammar is defined in §20.2. The top-level production is:
//!
//! ```text
//! AMLCode := DefBlockHeader TermList
//! ```
//!
//! Everything in `DefBlockHeader` just repeats the fixed
//! [`DescriptionHeader`](crate::DescriptionHeader) struct used in other tables,
//! so there's no reason to write a parser for it.

#![allow(clippy::wildcard_imports)]

use self::state::{ParserState, ReplaceableParseError};
use crate::aml::term::TermObject;
use crate::aml::AMLTable;
use crate::DescriptionHeader;
use alloc::vec;
use alloc::vec::Vec;
use core::convert::TryFrom;
use core::mem;
use nom::branch::alt;
use nom::bytes::complete as bytes;
use nom::combinator::{all_consuming, flat_map, map, opt, rest, value, verify};
use nom::error::{context, ErrorKind, ParseError};
use nom::multi;
use nom::sequence::{preceded, tuple};
use nom::IResult;
use tartan_parsers::error::{err, ErrorWithPosition};
use tartan_parsers::{opcode, struct_parser};


#[cfg(test)]
mod test;


/// Parse an ACPI table that contains AML, including the header
///
/// # Errors
/// Returns an error if the AML contents cannot be parsed.
pub fn parse_table(
    data: &[u8],
) -> Result<AMLTable<'_>, ErrorWithPosition<'_, ParserState<'_>>> {
    const HEADER_SIZE: usize = mem::size_of::<DescriptionHeader>();

    let state = ParserState::new(data);
    let mut body_parser =
        context("table body", all_consuming(multi::many0(TermObject::parse)));
    let parser = || {
        let (state, header_slice) = ParserState::lift(bytes::take(HEADER_SIZE))(state)?;
        let mut header_array = [0_u8; HEADER_SIZE];
        header_array.copy_from_slice(header_slice);
        // SAFETY: DescriptionHeader is composed of packed unsigned integers and is
        // trivially transmutable.
        let header: DescriptionHeader = unsafe { mem::transmute(header_array) };

        // Check data using header
        if header.length as usize != data.len() {
            return err(state, ErrorKind::Eof);
        }
        let checksum = data.iter().fold(0_u8, |a, b| a.wrapping_add(*b));
        if checksum != 0 {
            return err(state, ErrorKind::Verify);
        }

        let (state, body) = body_parser(state)?;

        Ok((state, AMLTable { header, body }))
    };
    match parser() {
        Ok((_, table)) => Ok(table),
        Err(e) => Err(ErrorWithPosition::new(e, data)),
    }
}


/// An object that can be parsed from AML bytecode
pub trait Parse<'a>
where
    Self: Sized,
{
    /// Try to parse an object of this type from the given input and state.
    ///
    /// # Errors
    /// Returns an error if the data cannot be parsed.
    fn parse<E: AMLParseError<'a>>(i: ParserState<'a>) -> AMLParseResult<Self, E>;
}


/// Alias for type returned by AML parsers
pub type AMLParseResult<'a, T, E> = IResult<ParserState<'a>, T, E>;


/// Trait "alias" for error type used by AML parsers
pub trait AMLParseError<'a>: ReplaceableParseError<&'a [u8], ParserState<'a>> {}

impl<'a, T> AMLParseError<'a> for T where
    T: ReplaceableParseError<&'a [u8], ParserState<'a>>
{
}


/// Parser context needed to disambiguate grammar
pub mod state {
    use super::super::name::{to_path, NameSeg, NameString};
    use super::*;
    use nom::error::VerboseError;
    use nom::{AsBytes, InputIter, InputLength, Needed, Parser};
    use tartan_parsers::error::GeneralParseError;


    /// Expected argument count for a method in the ACPI namespace.
    ///
    /// Needed because the grammar for method invocations is ambiguous without knowing the
    /// exact number of argument terms expected.
    #[derive(Debug, Default, Clone, PartialEq, Eq)]
    pub struct MethodSignature {
        pub name: Vec<NameSeg>,
        pub arg_count: u8,
    }

    impl MethodSignature {
        pub fn new<T: Copy + Into<NameSeg>>(name: &[T], arg_count: u8) -> Self {
            Self { name: to_path(name), arg_count }
        }
    }


    /// Input data plus context needed to disambiguate grammar
    #[derive(Debug, Default, Clone, PartialEq, Eq)]
    pub struct ParserState<'a> {
        pub data: &'a [u8],
        pub current_scope: Vec<NameSeg>,
        pub method_signatures: Vec<MethodSignature>,
    }

    impl<'a> ParserState<'a> {
        pub fn new(data: &'a [u8]) -> Self {
            Self { data, current_scope: vec![], method_signatures: vec![] }
        }

        /// Get the argument count of the named method, if it is declared
        pub fn get_arg_count(&self, name: &NameString) -> Option<u8> {
            let signature =
                name.resolve_as_ref(&self.current_scope).into_iter().find_map(
                    |abs_name| self.method_signatures.iter().find(|s| s.name == abs_name),
                )?;
            Some(signature.arg_count)
        }

        /// Execute a parser in a new scope relative to the current scope, then restore
        /// the original scope.
        ///
        /// # Errors
        ///
        /// Fails if the given scope is invalid when resolved against the current scope
        /// (because it falls outside the namespace root), or when the inner parser fails.
        pub fn in_scope<P, O, E>(
            self,
            new_scope: &NameString,
            mut parser: P,
        ) -> AMLParseResult<'a, O, E>
        where
            P: Parser<ParserState<'a>, O, E>,
            E: AMLParseError<'a>,
        {
            let Some(new_scope_abs) = new_scope.resolve_as_decl(&self.current_scope)
            else {
                return err(self, ErrorKind::Verify);
            };

            let previous_scope = self.current_scope.clone();
            let inner_state = ParserState { current_scope: new_scope_abs, ..self };

            let (inner_state, result) = parser.parse(inner_state)?;

            let outer_state =
                ParserState { current_scope: previous_scope, ..inner_state };
            Ok((outer_state, result))
        }

        /// Wraps a byte-oriented parser to work with `ParserState`
        pub fn lift<P, O, E>(
            mut parser: P,
        ) -> impl FnMut(ParserState<'a>) -> AMLParseResult<O, E>
        where
            P: Parser<&'a [u8], O, E::From>,
            E: AMLParseError<'a>,
        {
            move |input| match parser.parse(input.data) {
                Ok((output_data, output_value)) => {
                    Ok((ParserState { data: output_data, ..input }, output_value))
                }
                Err(nom::Err::Error(e)) => {
                    Err(nom::Err::Error(E::replace_input(e, input)))
                }
                Err(nom::Err::Failure(e)) => {
                    Err(nom::Err::Failure(E::replace_input(e, input)))
                }
                Err(nom::Err::Incomplete(e)) => Err(nom::Err::Incomplete(e)),
            }
        }
    }

    impl<'a> AsBytes for ParserState<'a> {
        fn as_bytes(&self) -> &[u8] {
            self.data
        }
    }

    impl<'a> From<&'a [u8]> for ParserState<'a> {
        fn from(data: &'a [u8]) -> Self {
            Self::new(data)
        }
    }

    impl<'a> InputIter for ParserState<'a> {
        type Item = <&'a [u8] as InputIter>::Item;
        type Iter = <&'a [u8] as InputIter>::Iter;
        type IterElem = <&'a [u8] as InputIter>::IterElem;
        fn iter_indices(&self) -> Self::Iter {
            self.data.iter_indices()
        }
        fn iter_elements(&self) -> Self::IterElem {
            self.data.iter_elements()
        }
        fn position<P: Fn(Self::Item) -> bool>(&self, pred: P) -> Option<usize> {
            self.data.position(pred)
        }
        fn slice_index(&self, i: usize) -> Result<usize, Needed> {
            self.data.slice_index(i)
        }
    }

    impl InputLength for ParserState<'_> {
        fn input_len(&self) -> usize {
            self.data.input_len()
        }
    }


    /// Helper trait used to convert a parser error from one input type to another.
    ///
    /// Used by [`ParserState::lift`].
    pub trait ReplaceableParseError<F, T>: GeneralParseError<T> {
        type From: GeneralParseError<F>;
        fn replace_input(from: Self::From, input: T) -> Self;
    }

    impl<F, T> ReplaceableParseError<F, T> for (T, ErrorKind) {
        type From = (F, ErrorKind);
        fn replace_input(from: Self::From, input: T) -> Self {
            (input, from.1)
        }
    }

    impl<F, T> ReplaceableParseError<F, T> for () {
        type From = ();
        fn replace_input(_: Self::From, _: T) -> Self {}
    }

    impl<F, T: Clone> ReplaceableParseError<F, T> for VerboseError<T> {
        type From = VerboseError<F>;
        fn replace_input(from: Self::From, input: T) -> Self {
            Self {
                // Replace *all* the inputs in the error list. This only makes sense if
                // the original error was close to the same point in the input. That is a
                // safe assumption in `ParserState::lift`, which is used primarily for
                // adapting low-level byte parsers.
                errors: from
                    .errors
                    .into_iter()
                    .map(|(_, kind)| (input.clone(), kind))
                    .collect(),
            }
        }
    }
}


/// Parsers from [`nom::number::complete`] wrapped to work with [`ParserState`].
///
/// The native parsers only work with byte slices.
mod num {
    #![allow(clippy::missing_errors_doc)]

    use super::*;
    use nom::number::complete as nom_num;

    /// Define a function that wraps a byte-oriented parser to work with [`ParserState`]
    macro_rules! lift {
        // Simple parser
        [
            $parser:ident
            -> $out:ty
            = $nom_parser:expr
        ] => {
            pub fn $parser<'a, E: AMLParseError<'a>>(
                i: ParserState<'a>
            ) -> AMLParseResult<$out, E> {
                // Hopefully rustc is smart enough to optimize away the closure
                ParserState::lift($nom_parser)(i)
            }
        };
    }

    lift!(le_u8  -> u8  = nom_num::le_u8);
    lift!(le_u16 -> u16 = nom_num::le_u16);
    lift!(le_u24 -> u32 = nom_num::le_u24);
    lift!(le_u32 -> u32 = nom_num::le_u32);
    lift!(le_u64 -> u64 = nom_num::le_u64);
}


#[macro_use]
mod util {
    use super::*;
    use nom::Parser;

    /// Defines a byte slice parser function, avoiding repetitive type parameters
    #[macro_export]
    macro_rules! parser_fn {
        // Shortcut assignment syntax
        [
            $( #[$meta:meta] )*
            $vis:vis $name:ident
                $( <$a:lifetime> )?
                -> $ret_typ:ty
                = $imp:expr
        ] => {
            parser_fn! {
                $(#[$meta])* $vis $name($($a)? i) -> $ret_typ {
                    #[allow(unused_mut)] // Sometimes needed, sometimes not
                    let mut parser = $imp;
                    parser(i)
                }
            }
        };

        // Use new lifetime parameter
        [
            $( #[$meta:meta] )*
            $vis:vis $name:ident ($input:ident)
                -> $ret_typ:ty
            $imp:block
        ] => {
            $(#[$meta])*
            $vis fn $name<'a, E: AMLParseError<'a>>(
                $input: ParserState<'a>
            ) -> AMLParseResult<$ret_typ, E> $imp
        };

        // Use lifetime from scope
        [
            $( #[$meta:meta] )*
            $vis:vis $name:ident ($a:lifetime $input:ident)
                -> $ret_typ:ty
            $imp:block
        ] => {
            $(#[$meta])*
            $vis fn $name<E: AMLParseError<$a>>(
                $input: ParserState<$a>
            ) -> AMLParseResult<$ret_typ, E> $imp
        };
    }

    /// Recognizes a single byte. Possibly slightly more efficient than `bytes::tag()`
    /// with a one-byte string, but I didn't actually profile it.
    pub fn tag_byte<'a, E: ParseError<ParserState<'a>>>(
        b: u8,
    ) -> impl FnMut(ParserState<'a>) -> AMLParseResult<u8, E> {
        move |i: ParserState<'_>| {
            if i.data.is_empty() {
                err(i, ErrorKind::Tag)
            } else if i.data[0] == b {
                let output = ParserState { data: &i.data[1..], ..i };
                Ok((output, b))
            } else {
                err(i, ErrorKind::Tag)
            }
        }
    }

    /// Combinator for two-byte extended opcodes
    ///
    /// Grammar defined under §20.2.3 ("Data Objects Encoding"):
    /// ```text
    /// ExtOpPrefix := 0x5B
    /// ```
    pub fn ext_op<'a, O, E, P>(
        p: P,
    ) -> impl FnMut(ParserState<'a>) -> AMLParseResult<O, E>
    where
        P: Parser<ParserState<'a>, O, E>,
        E: ParseError<ParserState<'a>>,
    {
        preceded(tag_byte(0x5b), p)
    }

    parser_fn! {
        /// Recognizes a null-terminated (C-style), possibly-empty 7-bit ASCII string.
        /// Strips the null terminator.
        pub c_ascii_str(i) -> &str {
            let (i, str_bytes) = ParserState::lift(
                bytes::take_till(|b: u8| b == 0 || !b.is_ascii())
            )(i)?;
            // Make sure we stopped at a null (not a non-ASCII byte or early end) and
            // consume it
            let (i, _) = tag_byte(0x00)(i)?;
            // SAFETY: We just checked that this is ASCII, so it's also valid UTF-8
            let str_utf8 = unsafe {
                core::str::from_utf8_unchecked(str_bytes)
            };
            Ok((i, str_utf8))
        }
    }
}


/// Name objects, defined in §20.2.2
pub mod name {
    use super::super::misc::*;
    use super::super::name::*;
    use super::super::term::ReferenceExpressionOpcode;
    use super::util::*;
    use super::*;
    use alloc::boxed::Box;

    /// Grammar:
    ///
    /// ```text
    /// NameSeg      := <LeadNameChar NameChar NameChar NameChar>
    /// LeadNameChar := ‘A’-‘Z’ | ‘_’
    /// DigitChar    := ‘0’-‘9’
    /// NameChar     := DigitChar | LeadNameChar
    /// ```
    impl<'a> Parse<'a> for NameSeg {
        parser_fn!(parse('a i) -> Self {
            const fn is_lead_name_char(b: u8) -> bool {
                #![allow(clippy::manual_range_contains)] // Spurious: not allowed in const
                b >= b'A' && b <= b'Z' || b == b'_'
            }

            const fn is_name_char(b: u8) -> bool {
                #![allow(clippy::manual_range_contains)] // Spurious: not allowed in const
                is_lead_name_char(b) || b >= b'0' && b <= b'9'
            }

            const fn is_name_seg(n: &[u8]) -> bool {
                n.len() >= 3
                    && is_lead_name_char(n[0])
                    && is_name_char(n[1])
                    && is_name_char(n[2])
                    && is_name_char(n[3])
            }

            let (i, n) = ParserState::lift(
                verify(bytes::take(4_usize), is_name_seg)
            )(i)?;
            Ok((i, NameSeg([n[0], n[1], n[2], n[3]])))
        });
    }


    /// Grammar:
    ///
    /// ```text
    /// NameString       := <RootChar NamePath> | <PrefixPath NamePath>
    /// PrefixPath       := Nothing | <‘^’ PrefixPath>
    /// NamePath         := NameSeg | DualNamePath | MultiNamePath | NullName
    ///
    /// RootChar         := ‘\’
    /// ParentPrefixChar := ‘^’
    ///
    /// NullName         := 0x00
    /// DualNamePath     := DualNamePrefix NameSeg NameSeg
    /// DualNamePrefix   := 0x2E
    /// MultiNamePath    := MultiNamePrefix SegCount NameSeg(SegCount)
    /// MultiNamePrefix  := 0x2F
    ///
    /// SegCount         := ByteData
    /// ```
    impl<'a> Parse<'a> for NameString {
        parser_fn! {
            parse<'a> -> Self = struct_parser! {
                NameString {
                    anchor: PathAnchor::parse,
                    path: alt((
                        map(NameSeg::parse, |s| vec![s]),
                        dual_name,
                        multi_name,
                        value(vec![], tag_byte(0x00)),
                    )),
                }
            }
        }
    }

    parser_fn! {
        /// See grammar for [`NameString`]
        dual_name -> Vec<NameSeg> = opcode(
            "name segments (2)",
            tag_byte(0x2e),
            multi::count(NameSeg::parse, 2),
        )
    }

    parser_fn! {
        /// See grammar for [`NameString`]
        multi_name -> Vec<NameSeg> = opcode(
            "name segments",
            tag_byte(0x2f),
            flat_map(num::le_u8, |n| multi::count(NameSeg::parse, n.into())),
        )
    }


    /// See grammar for [`NameString`].
    impl<'a> Parse<'a> for PathAnchor {
        parser_fn! {
            parse<'a> -> Self = alt((
                // Single root anchor, or
                value(Self::Root, tag_byte(b'\\')),
                // 0-n parent prefix anchors
                map(multi::many0_count(tag_byte(b'^')), Self::Parent),
            ))
        }
    }


    /// Grammar:
    ///
    /// ```text
    /// SimpleName := NameString | ArgObj | LocalObj
    /// ```
    impl<'a> Parse<'a> for SimpleName {
        parser_fn! {
            parse<'a> -> Self = alt((
                map(NameString::parse, Self::Name),
                map(ArgObject::parse, Self::Arg),
                map(LocalObject::parse, Self::Local),
            ))
        }
    }


    /// Grammar:
    ///
    /// ```text
    /// SuperName := SimpleName | DebugObj | Type6Opcode
    /// ```
    ///
    /// Despite the order given in the grammar, we have to try `Type6Opcode` **before**
    /// `SimpleName`. `Type6Opcode` includes method calls, which would never be reached if
    /// `SimpleName` slurped up the method name first.
    impl<'a> Parse<'a> for SuperName<'a> {
        parser_fn! {
            parse<'a> -> Self = alt((
                value(Self::Debug, DebugObject::parse),
                map(ReferenceExpressionOpcode::parse, |r| Self::Reference(Box::new(r))),
                map(SimpleName::parse, Self::Name),
            ))
        }
    }

    parser_fn! {
        /// Parse a [`SuperName`], but do *not* allow method invocations.
        ///
        /// This is needed by some operators where invocations are not allowed, but bare
        /// `NameString` references are---including method references. The standard
        /// `SuperName` parser would try to parse it as an invocation first.
        pub super_name_minus_invoke -> SuperName<'a> = alt((
            map(SimpleName::parse, SuperName::Name),
            value(SuperName::Debug, DebugObject::parse),
            // We don't need a special RefExpOp parser that excludes invocations. They
            // will be slurped up by the SimpleName parser first.
            map(ReferenceExpressionOpcode::parse, |r| SuperName::Reference(Box::new(r))),
        ))
    }

    parser_fn! {
        /// Grammar:
        ///
        /// ```text
        /// Target := SuperName | NullName
        /// ```
        pub parse_target -> Target = alt((
            value(None, tag_byte(0x00)),
            map(SuperName::parse, Some),
        ))
    }
}



/// Data objects, defined in §20.2.3
pub mod data {
    use super::super::data::*;
    use super::super::name::NameString;
    use super::super::term::TermArg;
    use super::package::in_package;
    use super::util::*;
    use super::*;

    /// Grammar:
    ///
    /// ```text
    /// ComputationalData := ByteConst | WordConst | DWordConst | QWordConst | String |
    ///                      ConstObj | RevisionOp | DefBuffer
    ///
    /// ByteConst         := BytePrefix ByteData
    /// BytePrefix        := 0x0A
    /// WordConst         := WordPrefix WordData
    /// WordPrefix        := 0x0B
    /// DWordConst        := DWordPrefix DWordData
    /// DWordPrefix       := 0x0C
    /// QWordConst        := QWordPrefix QWordData
    /// QWordPrefix       := 0x0E
    /// String            := StringPrefix AsciiCharList NullChar
    /// StringPrefix      := 0x0D
    ///
    /// ConstObj          := ZeroOp | OneOp | OnesOp
    /// ByteList          := Nothing | <ByteData ByteList>
    /// ByteData          := 0x00 - 0xFF
    /// WordData          := ByteData[0:7] ByteData[8:15]
    ///     // 0x0000-0xFFFF
    /// DWordData         := WordData[0:15] WordData[16:31]
    ///     // 0x00000000-0xFFFFFFFF
    /// QWordData         := DWordData[0:31] DWordData[32:63]
    ///     // 0x0000000000000000-0xFFFFFFFFFFFFFFFF
    /// AsciiCharList     := Nothing | <AsciiChar AsciiCharList>
    /// AsciiChar         := 0x01 - 0x7F
    /// NullChar          := 0x00
    /// ZeroOp            := 0x00
    /// OneOp             := 0x01
    /// OnesOp            := 0xFF
    /// RevisionOp        := ExtOpPrefix 0x30
    /// ```
    impl<'a> Parse<'a> for ComputationalData<'a> {
        parser_fn! {
            parse<'a> -> Self = alt((
                map(opcode("u8 literal",     tag_byte(0x0a), num::le_u8),  Self::Byte),
                map(opcode("u16 literal",    tag_byte(0x0b), num::le_u16), Self::Word),
                map(opcode("u32 literal",    tag_byte(0x0c), num::le_u32), Self::DWord),
                // NOTE: These two are out of order, but this is how the spec lists it
                map(opcode("u64 literal",    tag_byte(0x0e), num::le_u64), Self::QWord),
                map(opcode("string literal", tag_byte(0x0d), c_ascii_str), Self::String),
                value(Self::Zero, tag_byte(0x00)),
                value(Self::One, tag_byte(0x01)),
                value(Self::Ones, tag_byte(0xff)),
                value(Self::Revision, ext_op(tag_byte(0x30))),
                map(Buffer::parse, Self::Buffer),
            ))
        }
    }

    /// Grammar:
    ///
    /// ```text
    /// DefBuffer  := BufferOp PkgLength BufferSize ByteList
    /// BufferOp   := 0x11
    /// BufferSize := TermArg => Integer
    /// ```
    impl<'a> Parse<'a> for Buffer<'a> {
        parser_fn! {
            parse<'a> -> Self = opcode(
                "buffer",
                tag_byte(0x11),
                in_package(struct_parser! {
                    Buffer {
                        size: TermArg::parse,
                        initializer: ParserState::lift(rest),
                    }
                })
            )
        }
    }

    /// Grammar:
    ///
    /// ```text
    /// DefPackage  := PackageOp PkgLength NumElements PackageElementList
    /// PackageOp   := 0x12
    /// NumElements := ByteData
    /// ```
    impl<'a> Parse<'a> for Package<'a> {
        parser_fn! {
            parse<'a> -> Self = opcode(
                "package",
                tag_byte(0x12),
                in_package(struct_parser! {
                    Package {
                        count: num::le_u8,
                        initializers: multi::many0(PackageElement::parse),
                    }
                })
            )
        }
    }

    /// Grammar:
    ///
    /// ```text
    /// DefVarPackage  := VarPackageOp PkgLength VarNumElements PackageElementList
    /// VarPackageOp   := 0x13
    /// VarNumElements := TermArg => Integer
    /// ```
    impl<'a> Parse<'a> for VarPackage<'a> {
        parser_fn! {
            parse<'a> -> Self = opcode(
                "package (variable-length)",
                tag_byte(0x13),
                in_package(struct_parser! {
                    VarPackage {
                        count: TermArg::parse,
                        initializers: multi::many0(PackageElement::parse),
                    }
                })
            )
        }
    }

    /// Grammar:
    ///
    /// ```text
    /// PackageElementList := Nothing | <PackageElement PackageElementList>
    /// PackageElement     := DataRefObject | NameString
    /// ```
    impl<'a> Parse<'a> for PackageElement<'a> {
        parser_fn! {
            parse<'a> -> Self = alt((
                map(DataRefObject::parse, Self::Ref),
                map(NameString::parse, Self::Name),
            ))
        }
    }

    /// Grammar:
    ///
    /// ```text
    /// DataObject := ComputationalData | DefPackage | DefVarPackage
    /// ```
    impl<'a> Parse<'a> for DataObject<'a> {
        parser_fn! {
            parse<'a> -> Self = alt((
                map(ComputationalData::parse, Self::Data),
                map(Package::parse, Self::Package),
                map(VarPackage::parse, Self::VarPackage),
            ))
        }
    }

    /// Grammar:
    ///
    /// ```text
    /// DataRefObject := DataObject | ??ObjectReference | ??DDBHandle
    /// ```
    ///
    /// From ASL grammar (§19.2.4):
    ///
    /// ```text
    /// DDBHandle       := Integer
    /// ObjectReference := Integer
    /// ```
    ///
    /// However, it is unclear how these two types would be encoded in AML and how they
    /// would be distinguished from each other, or from the integral constants under
    /// `ComputationalData`. Consequently, they aren't implemented
    ///
    /// TODO: Figure out why this production exists, and get rid of it if it is no
    /// different than [`DataObject`].
    impl<'a> Parse<'a> for DataRefObject<'a> {
        parser_fn! {
            parse<'a> -> Self = map(DataObject::parse, DataRefObject::Data)
        }
    }
}


/// Variable-length "package" encoding, defined in §20.2.4
mod package {
    use super::*;
    use nom::Parser;

    /// The inner and outer size of a length-prefixed package
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct PackageLength {
        /// The total length of the package including the package length encoding itself.
        pub total_length: u32,

        /// The length of the remainder of the package following the length encoding.
        ///
        /// None if the total length is less than the size of the length encoding itself.
        pub body_length: Option<u32>,
    }

    impl<'a> Parse<'a> for PackageLength {
        parser_fn! {
            /// Grammar:
            /// ```text
            /// PkgLength :=
            ///     PkgLeadByte |
            ///     <PkgLeadByte ByteData> |
            ///     <PkgLeadByte ByteData ByteData> |
            ///     <PkgLeadByte ByteData ByteData ByteData>
            ///
            /// PkgLeadByte :=
            ///     <bit 7-6: ByteData count that follows (0-3)>
            ///     <bit 5-4: Only used if PkgLength < 63>
            ///     <bit 3-0: Least significant package length nybble>
            /// ```
            ///
            /// If multiple bytes are used, the least-significant nibble comes from the
            /// lower nibble `PkgLeadByte`, then the next two nibbles come from the next byte,
            /// and so on.
            parse('a i) -> PackageLength {
                let (i, head_byte) = num::le_u8(i)?;
                let tail_byte_count = head_byte >> 6;
                let length_low = u32::from(head_byte & 0x0f);
                let (i, length_high) = match tail_byte_count {
                    0 => (i, u32::from(head_byte & 0b0011_0000)),
                    1 => {
                        let (i, tail_byte) = num::le_u8(i)?;
                        (i, u32::from(tail_byte) << 4)
                    },
                    2 => {
                        let (i, tail_bytes) = num::le_u16(i)?;
                        (i, u32::from(tail_bytes) << 4)
                    },
                    3 => {
                        let (i, tail_bytes) = num::le_u24(i)?;
                        (i, tail_bytes << 4)
                    }
                    _ => panic!("Somehow got a value >= 4 from a two bit field"),
                };
                let total_length = length_high | length_low;
                let body_length = total_length.checked_sub((tail_byte_count + 1).into());
                Ok((i, PackageLength { total_length, body_length }))
            }
        }
    }

    parser_fn! {
        /// Parse a length value uses "package" encoding but is not actually followed by
        /// content.
        pub standalone_package_length -> u32 = map(
            PackageLength::parse,
            |l| l.total_length,
        )
    }

    /// Execute a parser inside a length-prefixed package
    pub fn in_package<'a, P, O, E>(
        inner_parser: P,
    ) -> impl FnMut(ParserState<'a>) -> AMLParseResult<O, E>
    where
        P: Parser<ParserState<'a>, O, E>,
        E: AMLParseError<'a>,
    {
        let mut consuming_inner_parser = all_consuming(inner_parser);
        move |outer_state: ParserState<'a>| {
            let (outer_state, package_length) = PackageLength::parse(outer_state)?;
            let Some(body_length) = package_length.body_length else {
                return err(outer_state, ErrorKind::Verify);
            };
            let (outer_state, package_data) =
                ParserState::lift(bytes::take(body_length))(outer_state)?;

            let inner_state = ParserState { data: package_data, ..outer_state.clone() };
            let (inner_state, parsed_package) = consuming_inner_parser(inner_state)?;
            let outer_state = ParserState { data: outer_state.data, ..inner_state };

            Ok((outer_state, parsed_package))
        }
    }
}


/// Terms, defined in §20.2.5
pub mod term {
    use super::super::data::{Buffer, DataObject, DataRefObject, Package, VarPackage};
    use super::super::misc::{ArgObject, LocalObject};
    use super::super::name::{NameSeg, NameString, SimpleName, SuperName};
    use super::super::term::*;
    use super::name::{parse_target, super_name_minus_invoke};
    use super::package::{in_package, standalone_package_length};
    use super::state::MethodSignature;
    use super::util::*;
    use super::*;
    use alloc::boxed::Box;


    /// Grammar:
    ///
    /// ```text
    /// TermObj  := Object | Type1Opcode | Type2Opcode
    /// TermList := Nothing | <TermObj TermList>
    /// Object   := NameSpaceModifierObj | NamedObj
    /// ```
    impl<'a> Parse<'a> for TermObject<'a> {
        parser_fn! {
            parse<'a> -> Self = alt((
                map(NameSpaceModifier::parse, |x| Self::Modifier(Box::new(x))),
                map(NamedObject::parse,       |x| Self::Named(Box::new(x))),
                map(StatementOpcode::parse,   |x| Self::Statement(Box::new(x))),
                map(ExpressionOpcode::parse,  |x| Self::Expression(Box::new(x))),
            ))
        }
    }


    /// Grammar:
    ///
    /// ```text
    /// TermArg     := Type2Opcode | DataObject | ArgObj | LocalObject
    /// TermArgList := Nothing | <TermArg TermArgList>
    /// ```
    ///
    /// Note that this syntax does not list `NameString`s as valid `TermArgs`, except as
    /// part of `MethodInvocation` under `Type6Opcode`. However, it's abundantly clear
    /// that names can be used as references to their values anywhere a term argument is
    /// expected. Tests with the ACPICA compiler confirm this. However, bare names are
    /// *not* allowed in `TermObj` positions, so it isn't appropriate to add them to
    /// `Type2Opcode` ([`ExpressionOpcode`]), and we have to add it to `TermArg` instead.
    impl<'a> Parse<'a> for TermArg<'a> {
        parser_fn! {
            parse<'a> -> Self = alt((
                map(ExpressionOpcode::parse, |x| Self::Expression(Box::new(x))),
                map(DataObject::parse,       |x| Self::Data(Box::new(x))),
                map(ArgObject::parse,            Self::Arg),
                map(LocalObject::parse,          Self::Local),
                map(NameString::parse,           Self::Name),
            ))
        }
    }


    /// Grammar:
    ///
    /// ```text
    /// NameSpaceModifierObj := DefAlias | DefName | DefScope
    /// ```
    impl<'a> Parse<'a> for NameSpaceModifier<'a> {
        parser_fn! {
            parse<'a> -> Self = alt((
                parse_alias,
                parse_name,
                parse_scope,
            ))
        }
    }

    parser_fn! {
        /// Grammar:
        ///
        /// ```text
        /// DefAlias := AliasOp NameString NameString
        /// AliasOp  := 0x06
        /// ```
        parse_alias -> NameSpaceModifier<'a> = opcode(
            "alias definition",
            tag_byte(0x06),
            |i| {
                let (i, source) = NameString::parse(i)?;
                let (mut i, alias) = NameString::parse(i)?;

                // Copy the method signature if the original name is a method
                if let Some(arg_count) = i.get_arg_count(&source) {
                    let Some(alias_abs) = alias.resolve_as_decl(&i.current_scope) else {
                        return err(i, ErrorKind::Verify)
                    };

                    i.method_signatures.push(MethodSignature {
                        name: alias_abs,
                        arg_count,
                    });
                }

                Ok((i, NameSpaceModifier::Alias { source, alias }))
            }
        )
    }

    parser_fn! {
        /// Grammar:
        ///
        /// ```text
        /// DefName := NameOp NameString DataRefObject
        /// NameOp  := 0x08
        /// ```
        parse_name -> NameSpaceModifier<'a> = opcode(
            "name definition",
            tag_byte(0x08),
            struct_parser! {
                NameSpaceModifier::Name(
                    NameString::parse,
                    DataRefObject::parse,
                )
            },
        )
    }

    parser_fn! {
        /// Grammar:
        ///
        /// ```text
        /// DefScope := ScopeOp PkgLength NameString TermList
        /// ScopeOp  := 0x10
        /// ```
        parse_scope -> NameSpaceModifier<'a> = opcode(
            "scope",
            tag_byte(0x10),
            in_package(|i| {
                let (i, name) = NameString::parse(i)?;
                let (i, body) = i.in_scope(&name, multi::many0(TermObject::parse))?;
                Ok((i, NameSpaceModifier::Scope(name, body)))
            })
        )
    }



    /// Grammar:
    ///
    /// ```text
    /// NamedObj := DefBankField | DefCreateBitField | DefCreateByteField |
    ///     DefCreateDWordField | DefCreateField | DefCreateQWordField |
    ///     DefCreateWordField | DefDataRegion | DefExternal | DefOpRegion | DefPowerRes |
    ///     DefProcessor | DefThermalZone
    /// ```
    ///
    /// **NOTE**: The AML grammar does *not* list the following alternatives as part of
    /// `NamedObj`, but they are defined in the same place as the others. They are also
    /// valid alternatives for the `NamedObject` production in the ASL grammar, so it is
    /// reasonable to assume they were omitted by mistake:
    ///   * `DefDevice`
    ///   * `DefEvent`
    ///   * `DefField`
    ///   * `DefIndexField`
    ///   * `DefMethod`
    ///   * `DefMutex`
    impl<'a> Parse<'a> for NamedObject<'a> {
        parser_fn! {
            parse<'a> -> Self = alt((
                bank_field,
                create_bit_field,
                create_byte_field,
                create_dword_field,
                create_field,
                create_qword_field,
                create_word_field,
                data_table_region,
                device,
                event,
                external,
                field,
                index_field,
                method,
                mutex,
                operation_region,
                power_resource,
                processor,
                thermal_zone,
            ))
        }
    }

    parser_fn! {
        /// Grammar:
        ///
        /// ```text
        /// DefBankField := BankFieldOp PkgLength NameString NameString BankValue FieldFlags FieldList
        /// BankFieldOp  := ExtOpPrefix 0x87
        /// BankValue    := TermArg => Integer
        /// ```
        bank_field -> NamedObject<'a> = opcode(
            "bank field group",
            ext_op(tag_byte(0x87)),
            in_package(struct_parser! {
                NamedObject::BankField {
                    region_name: NameString::parse,
                    bank_name: NameString::parse,
                    bank_value: TermArg::parse,
                    flags: FieldFlags::parse,
                    elements: multi::many0(FieldElement::parse),
                }
            })
        )
    }

    parser_fn! {
        /// Grammar:
        ///
        /// ```text
        /// DefCreateBitField    := CreateBitFieldOp SourceBuff BitIndex NameString
        /// CreateBitFieldOp     := 0x8D
        /// SourceBuff           := TermArg => Buffer
        /// BitIndex             := TermArg => Integer
        /// ```
        create_bit_field -> NamedObject<'a> = opcode(
            "bit field",
            tag_byte(0x8d),
            struct_parser! {
                NamedObject::CreateBitField {
                    source_buffer: TermArg::parse,
                    bit_index: TermArg::parse,
                    name: NameString::parse,
                }
            }
        )
    }

    macro_rules! parse_create_sized_field {
        [$(#[$meta:meta])* $parser_name:ident $struct:ident $desc:literal $tag:literal] => {
            parser_fn! {
                $(#[$meta])*
                $parser_name -> NamedObject<'a> = opcode(
                    $desc,
                    tag_byte($tag),
                    struct_parser! {
                        NamedObject::$struct {
                            source_buffer: TermArg::parse,
                            byte_index: TermArg::parse,
                            name: NameString::parse,
                        }
                    }
                )
            }
        };
    }

    parse_create_sized_field! {
        /// Grammar:
        ///
        /// ```text
        /// DefCreateByteField   := CreateByteFieldOp SourceBuff ByteIndex NameString
        /// CreateByteFieldOp    := 0x8C
        /// ByteIndex            := TermArg => Integer
        /// ```
        create_byte_field CreateByteField "u8 field" 0x8c
    }

    parse_create_sized_field! {
        /// Grammar:
        ///
        /// ```text
        /// DefCreateWordField   := CreateWordFieldOp SourceBuff ByteIndex NameString
        /// CreateWordFieldOp    := 0x8B
        /// ```
        create_word_field CreateWordField "u16 field" 0x8b
    }

    parse_create_sized_field! {
        /// Grammar:
        ///
        /// ```text
        /// DefCreateDWordField  := CreateDWordFieldOp SourceBuff ByteIndex NameString
        /// CreateDWordFieldOp   := 0x8A
        /// ```
        create_dword_field CreateDWordField "u32 field" 0x8a
    }

    parse_create_sized_field! {
        /// Grammar:
        ///
        /// ```text
        /// DefCreateQWordField  := CreateQWordFieldOp SourceBuff ByteIndex NameString
        /// CreateQWordFieldOp   := 0x8F
        /// ```
        create_qword_field CreateQWordField "u64 field" 0x8f
    }

    parser_fn! {
        /// Grammar:
        ///
        /// ```text
        /// DefCreateField       := CreateFieldOp SourceBuff BitIndex NumBits NameString
        /// CreateFieldOp        := ExtOpPrefix 0x13
        /// NumBits              := TermArg => Integer
        /// ```
        create_field -> NamedObject<'a> = opcode(
            "custom-length field",
            ext_op(tag_byte(0x13)),
            struct_parser! {
                NamedObject::CreateField {
                    source_buffer: TermArg::parse,
                    bit_index: TermArg::parse,
                    num_bits: TermArg::parse,
                    name: NameString::parse,
                }
            }
        )
    }

    parser_fn! {
        /// Grammar:
        ///
        /// ```text
        /// DefDataRegion        := DataRegionOp NameString TermArg TermArg TermArg
        /// DataRegionOp         := ExtOpPrefix 0x88
        /// ```
        data_table_region -> NamedObject<'a> = opcode(
            "data region",
            ext_op(tag_byte(0x88)),
            struct_parser! {
                NamedObject::DataTableRegion {
                    name: NameString::parse,
                    signature: TermArg::parse,
                    oem_id: TermArg::parse,
                    oem_table_id: TermArg::parse,
                }
            }
        )
    }

    parser_fn! {
        /// Grammar:
        ///
        /// ```text
        /// DefDevice            := DeviceOp PkgLength NameString TermList
        /// DeviceOp             := ExtOpPrefix 0x82
        /// ```
        device -> NamedObject<'a> = opcode(
            "device",
            ext_op(tag_byte(0x82)),
            in_package(|i| {
                let (i, name) = NameString::parse(i)?;
                let (i, body) = i.in_scope(&name, multi::many0(TermObject::parse))?;
                Ok((i, NamedObject::Device { name, body }))
            })
        )
    }

    parser_fn! {
        /// Grammar:
        ///
        /// ```text
        /// DefEvent             := EventOp NameString
        /// EventOp              := ExtOpPrefix 0x02
        /// ```
        event -> NamedObject<'a> = opcode(
            "event",
            ext_op(tag_byte(0x02)),
            map(NameString::parse, NamedObject::Event),
        )
    }

    parser_fn! {
        /// Grammar:
        ///
        /// ```text
        /// DefExternal          := ExternalOp NameString ObjectType ArgumentCount
        /// ExternalOp           := 0x15
        /// ObjectType           := ByteData
        /// ArgumentCount        := ByteData (0 – 7)
        /// ```
        external -> NamedObject<'a> = opcode(
            "external declaration",
            tag_byte(0x15),
            |i| {
                let (i, name) = NameString::parse(i)?;
                let (i, object_type) = ObjectType::parse(i)?;
                let (mut i, argument_count) = verify(num::le_u8, |b| *b <= 7)(i)?;

                if object_type == ObjectType::Method {
                    // Add the new method signature
                    let Some(abs_name) = name.resolve_as_decl(&i.current_scope) else {
                        return err(i, ErrorKind::Verify)
                    };
                    i.method_signatures.push(MethodSignature {
                        name: abs_name,
                        arg_count: argument_count,
                    });
                }

                Ok((i, NamedObject::External { name, object_type, argument_count }))
            }
        )
    }

    parser_fn! {
        /// Grammar:
        ///
        /// ```text
        /// DefField             := FieldOp PkgLength NameString FieldFlags FieldList
        /// FieldOp              := ExtOpPrefix 0x81
        /// ```
        field -> NamedObject<'a> = opcode(
            "field group",
            ext_op(tag_byte(0x81)),
            in_package(struct_parser! {
                NamedObject::Field {
                    region_name: NameString::parse,
                    flags: FieldFlags::parse,
                    elements: multi::many0(FieldElement::parse),
                }
            })
        )
    }

    parser_fn! {
        /// Grammar:
        ///
        /// ```text
        /// DefIndexField        := IndexFieldOp PkgLength NameString NameString FieldFlags FieldList
        /// IndexFieldOp         := ExtOpPrefix 0x86
        /// ```
        index_field -> NamedObject<'a> = opcode(
            "indexed field group",
            ext_op(tag_byte(0x86)),
            in_package(struct_parser! {
                NamedObject::IndexField {
                    index_name: NameString::parse,
                    data_name: NameString::parse,
                    flags: FieldFlags::parse,
                    elements: multi::many0(FieldElement::parse),
                }
            })
        )
    }

    parser_fn! {
        /// Grammar:
        ///
        /// ```text
        /// DefMethod            := MethodOp PkgLength NameString MethodFlags TermList
        /// MethodOp             := 0x14
        /// ```
        method -> NamedObject<'a> = opcode(
            "method definition",
            tag_byte(0x14),
            in_package(|i| {
                let (i, name) = NameString::parse(i)?;
                let (mut i, flags) = MethodFlags::parse(i)?;

                // Compute the absolute method name
                let outer_scope = i.current_scope.clone();
                let Some(abs_name) = name.resolve_as_decl(&outer_scope) else {
                    return err(i, ErrorKind::Verify)
                };

                // Add the new method signature
                i.method_signatures.push(MethodSignature {
                    name: abs_name.clone(),
                    arg_count: flags.arg_count,
                });

                // NOTE: Method scopes are weird. They are temporary, but still global.
                // Any objects they create are visible from outside... but only until the
                // method exits. If some insane person defined a method inside a method,
                // we will leave it inside our method signature list, but that doesn't
                // guarantee it's actually accessible. Leave that problem for runtime.
                i.current_scope = abs_name;
                let (mut i, body) = multi::many0(TermObject::parse)(i)?;
                i.current_scope = outer_scope;

                let method = NamedObject::Method {
                    name,
                    flags,
                    body,
                };
                Ok((i, method))
            })
        )
    }

    parser_fn! {
        /// Grammar:
        ///
        /// ```text
        /// DefMutex             := MutexOp NameString SyncFlags
        /// MutexOp              := ExtOpPrefix 0x01
        /// SyncFlags            := ByteData
        ///     // bit 0-3: SyncLevel (0x00-0x0f)
        ///     // bit 4-7: Reserved (must be 0)
        /// ```
        mutex -> NamedObject<'a> = opcode(
            "mutex",
            ext_op(tag_byte(0x01)),
            struct_parser! {
                NamedObject::Mutex {
                    name: NameString::parse,
                    sync_level: map(num::le_u8, |b| b & 0x0f),
                }
            }
        )
    }

    parser_fn! {
        /// Grammar:
        ///
        /// ```text
        /// DefOpRegion          := OpRegionOp NameString RegionSpace RegionOffset RegionLen
        /// OpRegionOp           := ExtOpPrefix 0x80
        /// RegionOffset         := TermArg => Integer
        /// RegionLen            := TermArg => Integer
        /// ```
        operation_region -> NamedObject<'a> = opcode(
            "region declaration",
            ext_op(tag_byte(0x80)),
            struct_parser! {
                NamedObject::OperationRegion {
                    name: NameString::parse,
                    region_space: RegionSpace::parse,
                    offset: TermArg::parse,
                    length: TermArg::parse,
                }
            }
        )
    }

    parser_fn! {
        /// Grammar:
        ///
        /// ```text
        /// DefPowerRes          := PowerResOp PkgLength NameString SystemLevel ResourceOrder TermList
        /// PowerResOp           := ExtOpPrefix 0x84
        /// SystemLevel          := ByteData
        /// ResourceOrder        := WordData
        /// ```
        power_resource -> NamedObject<'a> = opcode(
            "power resource",
            ext_op(tag_byte(0x84)),
            in_package(|i| {
                let (i, name) = NameString::parse(i)?;
                let (i, system_level) = num::le_u8(i)?;
                let (i, resource_order) = num::le_u16(i)?;
                let (i, body) = i.in_scope(&name, multi::many0(TermObject::parse))?;
                let resource = NamedObject::PowerResource {
                    name,
                    system_level,
                    resource_order,
                    body,
                };
                Ok((i, resource))
            })
        )
    }

    parser_fn! {
        /// Grammar:
        ///
        /// ```text
        /// DefProcessor         := ProcessorOp PkgLength NameString ProcID PblkAddr PblkLen TermList
        /// ProcessorOp          := ExtOpPrefix 0x83
        /// ProcID               := ByteData
        /// PblkAddr             := DWordData
        /// PblkLen              := ByteData
        /// ```
        processor -> NamedObject<'a> = opcode(
            "processor",
            ext_op(tag_byte(0x83)),
            in_package(|i| {
                let (i, name) = NameString::parse(i)?;
                let (i, id) = num::le_u8(i)?;
                let (i, register_block_addr) = num::le_u32(i)?;
                let (i, register_block_length) = num::le_u8(i)?;
                let (i, body) = i.in_scope(&name, multi::many0(TermObject::parse))?;
                let processor = NamedObject::Processor {
                    name,
                    id,
                    register_block_addr,
                    register_block_length,
                    body,
                };
                Ok((i, processor))
            })
        )
    }

    parser_fn! {
        /// Grammar:
        ///
        /// ```text
        /// DefThermalZone       := ThermalZoneOp PkgLength NameString TermList
        /// ThermalZoneOp        := ExtOpPrefix 0x85
        /// ```
        thermal_zone -> NamedObject<'a> = opcode(
            "thermal zone",
            ext_op(tag_byte(0x85)),
            in_package(|i| {
                let (i, name) = NameString::parse(i)?;
                let (i, body) = i.in_scope(&name, multi::many0(TermObject::parse))?;
                Ok((i, NamedObject::ThermalZone { name, body }))
            })
        )
    }


    /// Grammar:
    ///
    /// ```text
    /// FieldFlags := ByteData
    ///     // bit 0-3: AccessType
    ///     //          0    AnyAcc
    ///     //          1    ByteAcc
    ///     //          2    WordAcc
    ///     //          3    DWordAcc
    ///     //          4    QWordAcc
    ///     //          5    BufferAcc
    ///     //          6    Reserved
    ///     //          7-15 Reserved
    ///     // bit 4:   LockRule
    ///     //          0    NoLock
    ///     //          1    Lock
    ///     // bit 5-6: UpdateRule
    ///     //          0    Preserve
    ///     //          1    WriteAsOnes
    ///     //          2    WriteAsZeros
    ///     // bit 7:   Reserved (must be 0)
    /// ```
    impl<'a> Parse<'a> for FieldFlags {
        parser_fn!(parse('a i) -> Self {
            let (i, b) = num::le_u8(i)?;
            let Ok(access_type) = AccessType::try_from(b & 0x0f) else {
                return err(i, ErrorKind::Tag)
            };
            let lock = b & 0x10 != 0;
            let Ok(update_rule) = UpdateRule::try_from((b & 0b0110_0000) >> 5) else {
                return err(i, ErrorKind::Tag)
            };
            Ok((i, FieldFlags { access_type, lock, update_rule }))
        });
    }


    /// See grammar of [`FieldFlags`].
    ///
    /// This does **not** match the `AccessType` production in the AML grammar, which is
    /// really a hybrid between the `AccessType` and `AccessAttribute` productions from
    /// the ASL grammar. See `parse_access_field` for that rule in the AML grammar.
    ///
    /// `AccessType` does not implement Parser because it is always part of a bitfield.
    impl TryFrom<u8> for AccessType {
        type Error = ();

        fn try_from(b: u8) -> Result<Self, ()> {
            match b {
                0 => Ok(Self::Any),
                1 => Ok(Self::Byte),
                2 => Ok(Self::Word),
                3 => Ok(Self::DWord),
                4 => Ok(Self::QWord),
                5 => Ok(Self::Buffer),
                _ => Err(()),
            }
        }
    }


    /// See grammar of [`FieldFlags`].
    ///
    /// `UpdateRule` does not implement Parser because it is part of a bitfield.
    impl TryFrom<u8> for UpdateRule {
        type Error = ();

        fn try_from(b: u8) -> Result<Self, ()> {
            match b {
                0 => Ok(Self::Preserve),
                1 => Ok(Self::WriteAsOnes),
                2 => Ok(Self::WriteAsZeros),
                _ => Err(()),
            }
        }
    }


    /// Grammar:
    ///
    /// ```text
    /// FieldList     := Nothing | <FieldElement FieldList>
    /// FieldElement  := NamedField | ReservedField | AccessField | ExtendedAccessField |
    ///                  ConnectField
    ///
    /// NamedField    := NameSeg PkgLength
    /// ReservedField := 0x00 PkgLength
    /// ConnectField  := <0x02 NameString> | <0x02 ??BufferData>
    /// ```
    ///
    /// Notes:
    ///   * Although the ASL `Offset` grammar uses an absolute **byte** offset, the ACPICA
    ///     compiler seems to translate it into relative **bit** offset when it outputs
    ///     the `ReservedField` op in AML.
    ///   * `AccessField` and `ExtendedAccessField` encode the same information and are
    ///     merged into one enum variant. See `access_field` and `extended_access_field`.
    ///   * The ACPICA parser expects `BufferData` to be a `DefBuffer` op.
    ///   * The `ConnectField` op in split into two enum variants to avoid another level
    ///     of indirection.
    impl<'a> Parse<'a> for FieldElement<'a> {
        parser_fn! {
            parse<'a> -> Self = alt((
                context("named field", struct_parser!(Self::Named {
                        name: NameSeg::parse,
                        bit_length: standalone_package_length,
                })),
                opcode(
                    "reserved field",
                    tag_byte(0x00),
                    struct_parser!(Self::Reserved {
                        bit_length: standalone_package_length,
                    }),
                ),
                access_field,
                extended_access_field,
                opcode(
                    "connect field",
                    tag_byte(0x02),
                    alt((
                        map(NameString::parse, Self::ConnectNamed),
                        map(Buffer::parse,     Self::ConnectBuffer),
                    )),
                ),
            ))
        }
    }

    parser_fn! {
        /// Grammar:
        ///
        /// ```text
        /// AccessField := 0x01 AccessType AccessAttrib
        ///
        /// AccessType  := ByteData
        ///     // Bits 0:3 - Same as AccessType bits of FieldFlags.
        ///     // Bits 4:5 - Reserved
        ///     // Bits 7:6 - 0 = AccessAttrib = Normal Access Attributes
        ///     //            1 = AccessAttrib = AttribBytes (x)
        ///     //            2 = AccessAttrib = AttribRawBytes (x)
        ///     //            3 = AccessAttrib = AttribRawProcessBytes (x)
        ///     //            x' is encoded as bits 0:7 of the AccessAttrib byte.
        ///
        /// AccessAttrib  := ByteData
        ///     // If AccessType is BufferAcc for the SMB or
        ///     // GPIO OpRegions, AccessAttrib can be one of
        ///     // the following values:
        ///     //    0x02   AttribQuick
        ///     //    0x04   AttribSendReceive
        ///     //    0x06   AttribByte
        ///     //    0x08   AttribWord
        ///     //    0x0A   AttribBlock
        ///     //    0x0C   AttribProcessCall
        ///     //    0x0D   AttribBlockProcessCall
        /// ```
        access_field -> FieldElement<'a> = opcode(
            "field access modifiers",
            tag_byte(0x01),
            |i| {
                let (i, first) = num::le_u8(i)?;
                let (i, second) = num::le_u8(i)?;

                let Ok(access_type) = AccessType::try_from(first & 0x0f) else {
                    return err(i, ErrorKind::Tag)
                };

                let access_attrib = match first >> 6 {
                    1 => AccessAttrib::Bytes(second),
                    2 => AccessAttrib::RawBytes(second),
                    3 => AccessAttrib::RawProcessBytes(second),
                    0 => match second {
                        0x0 => AccessAttrib::None,  // Note that we added this value
                        0x2 => AccessAttrib::Quick,
                        0x4 => AccessAttrib::SendReceive,
                        0x6 => AccessAttrib::Byte,
                        0x8 => AccessAttrib::Word,
                        0xa => AccessAttrib::Block,
                        0xc => AccessAttrib::ProcessCall,
                        0xd => AccessAttrib::BlockProcessCall,
                        _ => return err(i, ErrorKind::Tag)
                    }
                    _ => panic!("Somehow got a value >= 4 from a two bit field"),
                };

                Ok((i, FieldElement::AccessAs(access_type, access_attrib)))
            }
        )
    }

    parser_fn! {
        /// The AML spec is kind of a mess here, and this production seems to encode the
        /// exact same information as `AccessField`, just not packed into bitfields. There
        /// also isn't anything explicitly preventing `ExtendedAccessField` from using the
        /// same packed representation that `AccessField` does, or from using the
        /// "non-extended" attribute values from the `AccessAttrib` production.
        ///
        /// Grammar:
        ///
        /// ```text
        /// ExtendedAccessField  := 0x03 AccessType ExtendedAccessAttrib ??AccessLength
        /// ExtendedAccessAttrib := ByteData
        ///     // 0x0B AttribBytes
        ///     // 0x0E AttribRawBytes
        ///     // 0x0F AttribRawProcess
        /// ```
        ///
        /// The ACPICA AML parser expects `AccessLength` to be a byte. See
        /// `source/components/parser/psargs.c`.
        extended_access_field -> FieldElement<'a> = opcode(
            "field access modifiers (extended)",
            tag_byte(0x03),
            |i| {
                let (i, type_byte) = num::le_u8(i)?;
                let (i, attrib_byte) = num::le_u8(i)?;
                let (i, length) = num::le_u8(i)?;

                let Ok(access_type) = AccessType::try_from(type_byte & 0x0f) else {
                    return err(i, ErrorKind::Tag)
                };

                // It's not clear whether it is legal to encode `Bytes`/`RawBytes`/
                // `RawProcessBytes` the way that the `AccessField` production does. Assume
                // that it isn't, and fail if the compiler tries.
                if type_byte >> 6 != 0 {
                    return err(i, ErrorKind::Tag);
                }

                let access_attrib = match attrib_byte {
                    0x0b => AccessAttrib::Bytes(length),
                    0x0e => AccessAttrib::RawBytes(length),
                    0x0f => AccessAttrib::RawProcessBytes(length),
                    // Also not clear whether the values from the `AccessAttrib` production
                    // are legal here. Again, assume that they aren't.
                    _ => return err(i, ErrorKind::Tag),
                };

                Ok((i, FieldElement::AccessAs(access_type, access_attrib)))
            }
        )
    }


    /// Grammar:
    ///
    /// ```text
    /// MethodFlags          := ByteData
    ///     // bit 0-2: ArgCount (0-7)
    ///     // bit 3:   SerializeFlag
    ///     //          0 NotSerialized
    ///     //          1 Serialized
    ///     // bit 4-7: SyncLevel (0x00-0x0f)
    /// ```
    impl<'a> Parse<'a> for MethodFlags {
        parser_fn! {
            parse<'a> -> Self = map(
                num::le_u8,
                |b| MethodFlags {
                    arg_count:  b & 0b0000_0111,
                    serialized: b & 0b0000_1000 != 0,
                    sync_level: b >> 4,
                }
            )
        }
    }


    /// Grammar:
    ///
    /// ```text
    /// RegionSpace          := ByteData
    ///     // 0x00       SystemMemory
    ///     // 0x01       SystemIO
    ///     // 0x02       PCI_Config
    ///     // 0x03       EmbeddedControl
    ///     // 0x04       SMBus
    ///     // 0x05       SystemCMOS
    ///     // 0x06       PciBarTarget
    ///     // 0x07       IPMI
    ///     // 0x08       GeneralPurposeIO
    ///     // 0x09       GenericSerialBus
    ///     // 0x0A       PCC
    ///     // 0x80-0xFF: OEM Defined
    /// ```
    impl<'a> Parse<'a> for RegionSpace {
        parser_fn!(parse('a i) -> Self {
            let (i, b) = num::le_u8(i)?;
            let value = match b {
                0x0 => Self::SystemMemory,
                0x1 => Self::SystemIO,
                0x2 => Self::PCIConfig,
                0x3 => Self::EmbeddedControl,
                0x4 => Self::SMBus,
                0x5 => Self::SystemCMOS,
                0x6 => Self::PCIBarTarget,
                0x7 => Self::IPMI,
                0x8 => Self::GeneralPurposeIO,
                0x9 => Self::GenericSerialBus,
                0xa => Self::PCC,
                0x80..=0xff => Self::OEMDefined(b),
                _ => return err(i, ErrorKind::Tag),
            };
            Ok((i, value))
        });
    }


    /// Grammar:
    ///
    /// ```text
    /// Type1Opcode := DefBreak | DefBreakPoint | DefContinue | DefFatal | DefIfElse |
    ///                DefLoad | DefNoop | DefNotify | DefRelease | DefReset | DefReturn |
    ///                DefSignal | DefSleep | DefStall | DefWhile
    /// ```
    impl<'a> Parse<'a> for StatementOpcode<'a> {
        parser_fn! {
            parse<'a> -> Self = alt((
                break_op,
                break_point,
                continue_op,
                fatal,
                if_op,
                load,
                no_op,
                notify,
                release,
                reset,
                return_op,
                signal,
                sleep,
                stall,
                while_op,
            ))
        }
    }

    parser_fn! {
        /// ```text
        /// DefBreak        := BreakOp
        /// BreakOp         := 0xA5
        /// ```
        break_op -> StatementOpcode<'a> = value(StatementOpcode::Break, tag_byte(0xa5))
    }

    parser_fn! {
        /// ```text
        /// DefBreakPoint   := BreakPointOp
        /// BreakPointOp    := 0xCC
        /// ```
        break_point -> StatementOpcode<'a> = value(StatementOpcode::BreakPoint, tag_byte(0xcc))
    }

    parser_fn! {
        /// ```text
        /// DefContinue     := ContinueOp
        /// ContinueOp      := 0x9F
        /// ```
        continue_op -> StatementOpcode<'a> = value(StatementOpcode::Continue, tag_byte(0x9f))
    }

    parser_fn! {
        /// ```text
        /// DefFatal        := FatalOp FatalType FatalCode FatalArg
        /// FatalOp         := ExtOpPrefix 0x32
        /// FatalType       := ByteData
        /// FatalCode       := DWordData
        /// FatalArg        := TermArg => Integer
        /// ```
        fatal -> StatementOpcode<'a> = opcode(
            "fatal statement",
            ext_op(tag_byte(0x32)),
            struct_parser! {
                StatementOpcode::Fatal {
                    fatal_type: num::le_u8,
                    code: num::le_u32,
                    arg: TermArg::parse,
                }
            }
        )
    }

    parser_fn! {
        /// ```text
        /// DefIfElse       := IfOp PkgLength Predicate TermList DefElse
        /// IfOp            := 0xA0
        /// Predicate       := TermArg => Integer
        /// DefElse         := Nothing | <ElseOp PkgLength TermList>
        /// ElseOp          := 0xA1
        /// ```
        if_op -> StatementOpcode<'a> = opcode(
            "if statement",
            tag_byte(0xa0),
            |i| {
                let (i, (predicate, if_true)) = in_package(tuple((
                    TermArg::parse,
                    multi::many0(TermObject::parse),
                )))(i)?;
                let (i, if_false) = opt(in_package(opcode(
                    "else branch",
                    tag_byte(0xa1),
                    multi::many0(TermObject::parse),
                )))(i)?;
                let if_op = StatementOpcode::If {
                    predicate,
                    if_true,
                    if_false: if_false.unwrap_or_default(),
                };
                Ok((i, if_op))
            }
        )
    }

    parser_fn! {
        /// ```text
        /// DefLoad         := LoadOp NameString DDBHandleObject
        /// LoadOp          := ExtOpPrefix 0x20
        /// DDBHandleObject := SuperName
        /// ```
        load -> StatementOpcode<'a> = opcode(
            "load statement",
            ext_op(tag_byte(0x20)),
            struct_parser! {
                StatementOpcode::Load {
                    name: NameString::parse,
                    definition_block_handle: SuperName::parse,
                }
            }
        )
    }

    parser_fn! {
        /// ```text
        /// DefNoop         := NoopOp
        /// NoopOp          := 0xA3
        /// ```
        no_op -> StatementOpcode<'a> = value(StatementOpcode::NoOp, tag_byte(0xa3))
    }

    parser_fn! {
        /// ```text
        /// DefNotify       := NotifyOp NotifyObject NotifyValue
        /// NotifyOp        := 0x86
        /// NotifyObject    := SuperName => ThermalZone | Processor | Device
        /// NotifyValue     := TermArg => Integer
        /// ```
        notify -> StatementOpcode<'a> = opcode(
            "notify statement",
            tag_byte(0x86),
            struct_parser! {
                StatementOpcode::Notify {
                    device_or_zone: SuperName::parse,
                    value: TermArg::parse,
                }
            }
        )
    }

    parser_fn! {
        /// ```text
        /// DefRelease      := ReleaseOp MutexObject
        /// ReleaseOp       := ExtOpPrefix 0x27
        /// MutexObject     := SuperName
        /// ```
        release -> StatementOpcode<'a> = opcode(
            "release statement",
            ext_op(tag_byte(0x27)),
            struct_parser! {
                StatementOpcode::Release { mutex: SuperName::parse }
            }
        )
    }

    parser_fn! {
        /// ```text
        /// DefReset        := ResetOp EventObject
        /// ResetOp         := ExtOpPrefix 0x26
        /// EventObject     := SuperName
        /// ```
        reset -> StatementOpcode<'a> = opcode(
            "reset statement",
            ext_op(tag_byte(0x26)),
            struct_parser! {
                StatementOpcode::Reset { event: SuperName::parse }
            }
        )
    }

    parser_fn! {
        /// ```text
        /// DefReturn       := ReturnOp ArgObject
        /// ReturnOp        := 0xA4
        /// ArgObject       := TermArg => DataRefObject
        /// ```
        return_op -> StatementOpcode<'a> = opcode(
            "return statement",
            tag_byte(0xa4),
            map(TermArg::parse, StatementOpcode::Return)
        )
    }

    parser_fn! {
        /// ```text
        /// DefSignal       := SignalOp EventObject
        /// SignalOp        := ExtOpPrefix 0x24
        /// ```
        signal -> StatementOpcode<'a> = opcode(
            "signal statement",
            ext_op(tag_byte(0x24)),
            struct_parser! {
                StatementOpcode::Signal { event: SuperName::parse }
            }
        )
    }

    parser_fn! {
        /// ```text
        /// DefSleep        := SleepOp MsecTime
        /// SleepOp         := ExtOpPrefix 0x22
        /// MsecTime        := TermArg => Integer
        /// ```
        sleep -> StatementOpcode<'a> = opcode(
            "sleep statement",
            ext_op(tag_byte(0x22)),
            struct_parser! {
                StatementOpcode::Sleep { milliseconds: TermArg::parse }
            }
        )
    }

    parser_fn! {
        /// ```text
        /// DefStall        := StallOp UsecTime
        /// StallOp         := ExtOpPrefix 0x21
        /// UsecTime        := TermArg => ByteData
        /// ```
        stall -> StatementOpcode<'a> = opcode(
            "stall statement",
            ext_op(tag_byte(0x21)),
            struct_parser! {
                StatementOpcode::Stall { microseconds: TermArg::parse }
            }
        )
    }

    parser_fn! {
        /// ```text
        /// DefWhile        := WhileOp PkgLength Predicate TermList
        /// WhileOp         := 0xA2
        /// ```
        while_op -> StatementOpcode<'a> = opcode(
            "while loop",
            tag_byte(0xa2),
            in_package(struct_parser! {
                StatementOpcode::While {
                    predicate: TermArg::parse,
                    body: multi::many0(TermObject::parse),
                }
            })
        )
    }


    /// Grammar:
    ///
    /// ```text
    /// Type6Opcode := DefRefOf | DefDerefOf | DefIndex | ??UserTermObj
    /// ```
    ///
    /// An explanation in the ASL grammar seems to indicate that `UserTermObj` is
    /// synonymous with `MethodInvocation`.
    impl<'a> Parse<'a> for ReferenceExpressionOpcode<'a> {
        parser_fn! {
            parse<'a> -> Self = alt((
                ref_of,
                deref,
                index,
                invoke,
            ))
        }
    }

    parser_fn! {
        /// ```text
        /// DefRefOf            := RefOfOp SuperName
        /// RefOfOp             := 0x71
        /// ```
        ///
        /// Although the grammar suggests all SuperNames are accepted, the ACPICA parser
        /// does not allow method invocations or nested `RefOf` operators.
        ref_of -> ReferenceExpressionOpcode<'a> = opcode(
            "reference-of operator",
            tag_byte(0x71),
            map(
                verify(
                    super_name_minus_invoke,
                    // Forbid RefOf(RefOf(_))
                    |n| match n {
                        SuperName::Reference(r) =>
                            !matches!(**r, ReferenceExpressionOpcode::RefOf(_)),
                        _ => true,
                    }
                ),
                ReferenceExpressionOpcode::RefOf,
            ),
        )
    }

    parser_fn! {
        /// ```text
        /// DefDerefOf          := DerefOfOp ObjReference
        /// DerefOfOp           := 0x83
        /// ObjReference        := TermArg => ??ObjectReference | String
        /// ```
        deref -> ReferenceExpressionOpcode<'a> = opcode(
            "dereference operator",
            tag_byte(0x83),
            map(TermArg::parse, ReferenceExpressionOpcode::Deref),
        )
    }

    parser_fn! {
        /// ```text
        /// DefIndex            := IndexOp BuffPkgStrObj IndexValue Target
        /// IndexOp             := 0x88
        /// BuffPkgStrObj       := TermArg => Buffer, Package or String
        /// IndexValue          := TermArg => Integer
        /// ```
        index -> ReferenceExpressionOpcode<'a> = opcode(
            "index operator",
            tag_byte(0x88),
            struct_parser! {
                ReferenceExpressionOpcode::Index {
                    source: TermArg::parse,
                    index: TermArg::parse,
                    result: parse_target,
                }
            }
        )
    }

    parser_fn! {
        /// ```text
        /// MethodInvocation := NameString TermArgList
        /// ```
        invoke(i) -> ReferenceExpressionOpcode<'a> {
            let (i, name) = NameString::parse(i)?;

            let Some(arg_count) = i.get_arg_count(&name) else {
                return err(i, ErrorKind::Verify)
            };

            let (i, args) = multi::count(TermArg::parse, arg_count.into())(i)?;

            Ok((i, ReferenceExpressionOpcode::Invoke(name, args)))
        }
    }


    /// Grammar:
    ///
    /// ```text
    /// Type2Opcode := DefAcquire | DefAdd | DefAnd | DefBuffer | DefConcat |
    ///                DefConcatRes | DefCondRefOf | DefCopyObject | DefDecrement |
    ///                DefDerefOf | DefDivide | DefFindSetLeftBit | DefFindSetRightBit |
    ///                DefFromBCD | DefIncrement | DefIndex | DefLAnd | DefLEqual |
    ///                DefLGreater | DefLGreaterEqual | DefLLess | DefLLessEqual | DefMid |
    ///                DefLNot | DefLNotEqual | DefLoadTable | DefLOr | DefMatch | DefMod |
    ///                DefMultiply | DefNAnd | DefNOr | DefNot | DefObjectType | DefOr |
    ///                DefPackage | DefVarPackage | DefRefOf | DefShiftLeft |
    ///                DefShiftRight | DefSizeOf | DefStore | DefSubtract | DefTimer |
    ///                DefToBCD | DefToBuffer | DefToDecimalString | DefToHexString |
    ///                DefToInteger | DefToString | DefWait | DefXOr | MethodInvocation
    /// ```
    impl<'a> Parse<'a> for ExpressionOpcode<'a> {
        parser_fn! {
            parse<'a> -> Self = alt((
                // Broken up in groups of 20 to avoid alt()'s parameter limit
                alt((
                    map(ReferenceExpressionOpcode::parse, Self::RefExpression),
                    map(Buffer::parse, Self::Buffer),
                    map(Package::parse, Self::Package),
                    map(VarPackage::parse, Self::VarPackage),
                    acquire,
                    add,
                    bitwise_and,
                    concat,
                    concat_res,
                    cond_ref_of,
                    copy_object,
                    decrement,
                    divide,
                    find_set_left_bit,
                    find_set_right_bit,
                    from_bcd,
                    increment,
                    logical_and,
                    equal,
                    greater,
                )),
                alt((
                    // covers !, !=, >=, <=
                    inverted_logical_ops,
                    less,
                    load_table,
                    logical_or,
                    match_op,
                    mid,
                    mod_op,
                    multiply,
                    nand,
                    nor,
                    bitwise_not,
                    object_type,
                    bitwise_or,
                    shift_left,
                    shift_right,
                    size_of,
                    store,
                    subtract,
                    timer,
                    to_bcd,
                )),
                alt((
                    to_buffer,
                    to_decimal_string,
                    to_hex_string,
                    to_integer,
                    to_string,
                    wait,
                    bitwise_xor,
                )),
            ))
        }
    }

    macro_rules! binary_op_parser {
        [$( #[$meta:meta] )* $parser:ident $op:ident $desc:literal $tag:literal] => {
            parser_fn! {
                $(#[$meta])*
                $parser -> ExpressionOpcode<'a> = opcode(
                    $desc,
                    tag_byte($tag),
                    struct_parser! {
                        ExpressionOpcode::$op(TermArg::parse, TermArg::parse, parse_target)
                    }
                )
            }
        };
    }

    binary_op_parser! {
        /// ```text
        /// DefAdd              := AddOp Operand Operand Target
        /// AddOp               := 0x72
        /// Operand             := TermArg => Integer
        /// ```
        add Add "add operator" 0x72
    }

    binary_op_parser! {
        /// ```text
        /// DefAnd              := AndOp Operand Operand Target
        /// AndOp               := 0x7B
        /// ```
        bitwise_and BitwiseAnd "bitwise AND operator" 0x7b
    }

    binary_op_parser! {
        /// ```text
        /// DefOr               := OrOp Operand Operand Target
        /// OrOp                := 0x7D
        /// ```
        bitwise_or BitwiseOr "bitwise OR operator" 0x7d
    }

    binary_op_parser! {
        /// ```text
        /// DefXOr              := XorOp Operand Operand Target
        /// XorOp               := 0x7F
        /// ```
        bitwise_xor BitwiseXor "bitwise XOR operator" 0x7f
    }

    binary_op_parser! {
        /// ```text
        /// DefConcat           := ConcatOp Data Data Target
        /// ConcatOp            := 0x73
        /// Data                := TermArg => ComputationalData
        /// ```
        concat Concat "concat operator" 0x73
    }

    binary_op_parser! {
        /// ```text
        /// DefConcatRes        := ConcatResOp BufData BufData Target
        /// ConcatResOp         := 0x84
        /// BufData             := TermArg => Buffer
        /// ```
        concat_res ConcatRes "concat resource template operator" 0x84
    }

    binary_op_parser! {
        /// ```text
        /// DefMod              := ModOp Dividend Divisor Target
        /// ModOp               := 0x85
        /// ```
        mod_op Mod "modulus operator" 0x85
    }

    binary_op_parser! {
        /// ```text
        /// DefMultiply         := MultiplyOp Operand Operand Target
        /// MultiplyOp          := 0x77
        /// ```
        multiply Multiply "multiplication operator" 0x77
    }

    binary_op_parser! {
        /// ```text
        /// DefNAnd             := NandOp Operand Operand Target
        /// NandOp              := 0x7C
        /// ```
        nand Nand "bitwise NAND operator" 0x7c
    }

    binary_op_parser! {
        /// ```text
        /// DefNOr              := NorOp Operand Operand Target
        /// NorOp               := 0x7E
        /// ```
        nor Nor "bitwise NOR operator" 0x7e
    }

    binary_op_parser! {
        /// ```text
        /// DefShiftLeft        := ShiftLeftOp Operand ShiftCount Target
        /// ShiftLeftOp         := 0x79
        /// ShiftCount          := TermArg => Integer
        /// ```
        shift_left ShiftLeft "left shift operator" 0x79
    }

    binary_op_parser! {
        /// ```text
        /// DefShiftRight       := ShiftRightOp Operand ShiftCount Target
        /// ShiftRightOp        := 0x7A
        /// ```
        shift_right ShiftRight "right shift operator" 0x7a
    }

    binary_op_parser! {
        /// ```text
        /// DefSubtract         := SubtractOp Operand Operand Target
        /// SubtractOp          := 0x74
        /// ```
        subtract Subtract "subtraction operator" 0x74
    }

    macro_rules! unary_op_parser {
        [$( #[$meta:meta] )* $parser:ident $op:ident $desc:literal $tag:literal] => {
            unary_op_parser! { $(#[$meta])* $parser $op $desc tag_byte($tag) }
        };

        [$( #[$meta:meta] )* $parser:ident $op:ident $desc:literal $tag:expr] => {
            parser_fn! {
                $(#[$meta])*
                $parser -> ExpressionOpcode<'a> = opcode(
                    $desc,
                    $tag,
                    struct_parser! {
                        ExpressionOpcode::$op(TermArg::parse, parse_target)
                    }
                )
            }
        };
    }

    unary_op_parser! {
        /// ```text
        /// DefNot              := NotOp Operand Target
        /// NotOp               := 0x80
        /// ```
        bitwise_not BitwiseNot "bitwise NOT operator" 0x80
    }

    unary_op_parser! {
        /// ```text
        /// DefFindSetLeftBit   := FindSetLeftBitOp Operand Target
        /// FindSetLeftBitOp    := 0x81
        /// ```
        find_set_left_bit FindSetLeftBit "find left bit operator" 0x81
    }

    unary_op_parser! {
        /// ```text
        /// DefFindSetRightBit  := FindSetRightBitOp Operand Target
        /// FindSetRightBitOp   := 0x82
        /// ```
        find_set_right_bit FindSetRightBit "find right bit operator" 0x82
    }

    unary_op_parser! {
        /// ```text
        /// DefFromBCD          := FromBCDOp BCDValue Target
        /// FromBCDOp           := ExtOpPrefix 0x28
        /// ```
        from_bcd FromBCD "from BCD conversion" ext_op(tag_byte(0x28))
    }

    unary_op_parser! {
        /// ```text
        /// DefToBCD            := ToBCDOp Operand Target
        /// ToBCDOp             := ExtOpPrefix 0x29
        /// BCDValue            := TermArg => Integer
        /// ```
        to_bcd ToBCD "to BCD conversion" ext_op(tag_byte(0x29))
    }

    unary_op_parser! {
        /// ```text
        /// DefToBuffer         := ToBufferOp Operand Target
        /// ToBufferOp          := 0x96
        /// ```
        to_buffer ToBuffer "buffer conversion" 0x96
    }

    unary_op_parser! {
        /// ```text
        /// DefToDecimalString  := ToDecimalStringOp Operand Target
        /// ToDecimalStringOp   := 0x97
        /// ```
        to_decimal_string ToDecimalString "decimal string conversion" 0x97
    }

    unary_op_parser! {
        /// ```text
        /// DefToHexString      := ToHexStringOp Operand Target
        /// ToHexStringOp       := 0x98
        /// ```
        to_hex_string ToHexString "hex string conversion" 0x98
    }

    unary_op_parser! {
        /// ```text
        /// DefToInteger        := ToIntegerOp Operand Target
        /// ToIntegerOp         := 0x99
        /// ```
        to_integer ToInteger "integer conversion" 0x99
    }

    macro_rules! logical_op_parser {
        [$( #[$meta:meta] )* $parser:ident $op:ident $desc:literal $tag:literal] => {
            parser_fn! {
                $(#[$meta])*
                $parser -> ExpressionOpcode<'a> = opcode(
                    $desc,
                    tag_byte($tag),
                    struct_parser! {
                        ExpressionOpcode::$op(TermArg::parse, TermArg::parse)
                    }
                )
            }
        };
    }

    logical_op_parser! {
        /// ```text
        /// DefLEqual           := LequalOp Operand Operand
        /// LequalOp            := 0x93
        /// ```text
        equal Equal "equality operator" 0x93
    }

    logical_op_parser! {
        /// ```text
        /// DefLGreater         := LgreaterOp Operand Operand
        /// LgreaterOp          := 0x94
        /// ```text
        greater Greater "greater-than operator" 0x94
    }

    logical_op_parser! {
        /// ```text
        /// DefLLess            := LlessOp Operand Operand
        /// LlessOp             := 0x95
        /// ```text
        less Less "less-than operator" 0x95
    }

    logical_op_parser! {
        /// ```text
        /// DefLAnd             := LandOp Operand Operand
        /// LandOp              := 0x90
        /// ```text
        logical_and LogicalAnd "logical AND operator " 0x90
    }

    logical_op_parser! {
        /// ```text
        /// DefLOr              := LorOp Operand Operand
        /// LorOp               := 0x91
        /// ```text
        logical_or LogicalOr "logical OR operator" 0x91
    }

    parser_fn! {
        /// This implements both pure "not" **and** the inverted comparison operators,
        /// which are really just parser optimizations.
        ///
        /// ```text
        /// DefLNot             := LnotOp Operand
        /// LnotOp              := 0x92
        /// DefLGreaterEqual    := LgreaterEqualOp Operand Operand
        /// LgreaterEqualOp     := LnotOp LlessOp
        /// DefLLessEqual       := LlessEqualOp Operand Operand
        /// LlessEqualOp        := LnotOp LgreaterOp
        /// DefLNotEqual        := LnotEqualOp Operand Operand
        /// LnotEqualOp         := LnotOp LequalOp
        /// ```text
        inverted_logical_ops -> ExpressionOpcode<'a> = opcode(
            "logical NOT operator",
            tag_byte(0x92),
            alt((
                opcode(
                    "less-than operator",
                    tag_byte(0x95),
                    struct_parser!(ExpressionOpcode::GreaterEqual(TermArg::parse, TermArg::parse)),
                ),
                opcode(
                    "greater-than operator",
                    tag_byte(0x94),
                    struct_parser!(ExpressionOpcode::LessEqual(TermArg::parse, TermArg::parse)),
                ),
                opcode(
                    "equality operator",
                    tag_byte(0x93),
                    struct_parser!(ExpressionOpcode::NotEqual(TermArg::parse, TermArg::parse)),
                ),
                map(TermArg::parse, ExpressionOpcode::LogicalNot),
            ))
        )
    }

    parser_fn! {
        /// ```text
        /// DefDecrement        := DecrementOp SuperName
        /// DecrementOp         := 0x76
        /// ```
        decrement -> ExpressionOpcode<'a> = opcode(
            "decrement operator",
            tag_byte(0x76),
            map(SuperName::parse, ExpressionOpcode::Decrement),
        )
    }

    parser_fn! {
        /// ```text
        /// DefIncrement        := IncrementOp SuperName
        /// IncrementOp         := 0x75
        /// ```
        increment -> ExpressionOpcode<'a> = opcode(
            "increment operator",
            tag_byte(0x75),
            map(SuperName::parse, ExpressionOpcode::Increment),
        )
    }

    parser_fn! {
        /// ```text
        /// DefSizeOf           := SizeOfOp SuperName
        /// SizeOfOp            := 0x87
        /// ```
        size_of -> ExpressionOpcode<'a> = opcode(
            "size-of operator",
            tag_byte(0x87),
            map(SuperName::parse, ExpressionOpcode::SizeOf),
        )
    }

    parser_fn! {
        /// ```text
        /// DefObjectType       := ObjectTypeOp <SimpleName | DebugObj |
        ///                        DefRefOf | DefDerefOf | DefIndex>
        /// ObjectTypeOp        := 0x8E
        /// ```
        ///
        /// NOTE: SuperName includes MethodInvocation, which is *not* legal for the
        /// ObjectType operator. It is otherwise identical to the grammar above.
        object_type -> ExpressionOpcode<'a> = opcode(
            "object type operator",
            tag_byte(0x8e),
            map(super_name_minus_invoke, ExpressionOpcode::ObjectType),
        )
    }

    parser_fn! {
        /// ```text
        /// DefTimer            := TimerOp
        /// TimerOp             := 0x5B 0x33
        /// ```
        timer -> ExpressionOpcode<'a> = value(
            ExpressionOpcode::Timer,
            ext_op(tag_byte(0x33))
        )
    }

    parser_fn! {
        /// ```text
        /// DefAcquire          := AcquireOp MutexObject Timeout
        /// AcquireOp           := ExtOpPrefix 0x23
        /// Timeout             := WordData
        /// ```
        acquire -> ExpressionOpcode<'a> = opcode(
            "acquire expression",
            ext_op(tag_byte(0x23)),
            struct_parser! {
                ExpressionOpcode::Acquire {
                    mutex: SuperName::parse,
                    timeout: num::le_u16,
                }
            }
        )
    }

    parser_fn! {
        /// ```text
        /// DefWait             := WaitOp EventObject Operand
        /// WaitOp              := ExtOpPrefix 0x25
        /// ```
        wait -> ExpressionOpcode<'a> = opcode(
            "wait expression",
            ext_op(tag_byte(0x25)),
            struct_parser! {
                ExpressionOpcode::Wait {
                    event: SuperName::parse,
                    timeout: TermArg::parse,
                }
            }
        )
    }

    parser_fn! {
        /// ```text
        /// DefCondRefOf        := CondRefOfOp SuperName Target
        /// CondRefOfOp         := ExtOpPrefix 0x12
        /// ```
        ///
        /// Like [`ReferenceExpressionOpcode::RefOf`], method invocations and nested
        /// `RefOf` calls are not allowed in the first argument even though the spec
        /// suggests they are.
        cond_ref_of -> ExpressionOpcode<'a> = opcode(
            "reference-of operator (conditional)",
            ext_op(tag_byte(0x12)),
            struct_parser! {
                ExpressionOpcode::CondRefOf(
                    verify(
                        super_name_minus_invoke,
                        // Forbid RefOf(RefOf(_))
                        |n| match n {
                            SuperName::Reference(r) =>
                                !matches!(**r, ReferenceExpressionOpcode::RefOf(_)),
                            _ => true,
                        }
                    ),
                    parse_target,
                )
            }
        )
    }

    parser_fn! {
        /// ```text
        /// DefCopyObject       := CopyObjectOp TermArg SimpleName
        /// CopyObjectOp        := 0x9D
        /// ```
        copy_object -> ExpressionOpcode<'a> = opcode(
            "copy expression",
            tag_byte(0x9d),
            struct_parser! {
                ExpressionOpcode::CopyObject(TermArg::parse, SimpleName::parse)
            }
        )
    }

    parser_fn! {
        /// ```text
        /// DefDivide           := DivideOp Dividend Divisor Remainder Quotient
        /// DivideOp            := 0x78
        /// Dividend            := TermArg => Integer
        /// Divisor             := TermArg => Integer
        /// Remainder           := Target
        /// Quotient            := Target
        /// ```
        divide -> ExpressionOpcode<'a> = opcode(
            "division operator",
            tag_byte(0x78),
            struct_parser! {
                ExpressionOpcode::Divide {
                    dividend: TermArg::parse,
                    divisor: TermArg::parse,
                    remainder: parse_target,
                    quotient: parse_target,
                }
            }
        )
    }

    parser_fn! {
        /// ```text
        /// DefLoadTable        := LoadTableOp TermArg TermArg TermArg TermArg TermArg TermArg
        /// LoadTableOp         := ExtOpPrefix 0x1F
        /// ```
        load_table -> ExpressionOpcode<'a> = opcode(
            "load table expression",
            ext_op(tag_byte(0x1f)),
            struct_parser! {
                ExpressionOpcode::LoadTable {
                    signature: TermArg::parse,
                    oem_id: TermArg::parse,
                    oem_table_id: TermArg::parse,
                    root_path: TermArg::parse,
                    parameter_path: TermArg::parse,
                    parameter_data: TermArg::parse,
                }
            }
        )
    }

    parser_fn! {
        /// ```text
        /// DefMatch            := MatchOp SearchPkg MatchOpcode Operand MatchOpcode Operand StartIndex
        /// MatchOp             := 0x89
        /// SearchPkg           := TermArg => Package
        /// StartIndex          := TermArg => Integer
        /// ```
        match_op -> ExpressionOpcode<'a> = opcode(
            "match expression",
            tag_byte(0x89),
            struct_parser! {
                ExpressionOpcode::Match {
                    search_package: TermArg::parse,
                    a: tuple((MatchOpcode::parse, TermArg::parse)),
                    b: tuple((MatchOpcode::parse, TermArg::parse)),
                    start_index: TermArg::parse,
                }
            }
        )
    }

    parser_fn! {
        /// ```text
        /// DefMid              := MidOp MidObj TermArg TermArg Target
        /// MidOp               := 0x9E
        /// MidObj              := TermArg => Buffer | String
        /// ```
        mid -> ExpressionOpcode<'a> = opcode(
            "mid operator",
            tag_byte(0x9e),
            struct_parser! {
                ExpressionOpcode::Mid {
                    source: TermArg::parse,
                    index: TermArg::parse,
                    length: TermArg::parse,
                    result: parse_target,
                }
            }
        )
    }

    parser_fn! {
        /// ```text
        /// DefStore            := StoreOp TermArg SuperName
        /// StoreOp             := 0x70
        /// ```
        store -> ExpressionOpcode<'a> = opcode(
            "store expression",
            tag_byte(0x70),
            struct_parser! {
                ExpressionOpcode::Store(TermArg::parse, SuperName::parse)
            }
        )
    }

    parser_fn! {
        /// ```text
        /// DefToString         := ToStringOp TermArg LengthArg Target
        /// LengthArg           := TermArg => Integer
        /// ToStringOp          := 0x9C
        /// ```
        to_string -> ExpressionOpcode<'a> = opcode(
            "string conversion",
            tag_byte(0x9c),
            struct_parser! {
                ExpressionOpcode::ToString {
                    source: TermArg::parse,
                    length: TermArg::parse,
                    result: parse_target,
                }
            }
        )
    }

    /// Grammar:
    ///
    /// ```text
    /// MatchOpcode := ByteData
    ///     // 0 MTR
    ///     // 1 MEQ
    ///     // 2 MLE
    ///     // 3 MLT
    ///     // 4 MGE
    ///     // 5 MGT
    /// ```
    impl<'a> Parse<'a> for MatchOpcode {
        parser_fn!(parse('a i) -> Self {
            let (i, b) = num::le_u8(i)?;
            let value = match b {
                0 => Self::True,
                1 => Self::Equal,
                2 => Self::LessEqual,
                3 => Self::Less,
                4 => Self::GreaterEqual,
                5 => Self::Greater,
                _ => return err(i, ErrorKind::Tag),
            };
            Ok((i, value))
        });
    }


    /// Values from Table 19-433:
    ///
    /// ```text
    ///   0   Uninitialized
    ///   1   Integer
    ///   2   String
    ///   3   Buffer
    ///   4   Package
    ///   5   Field Unit
    ///   6   Device
    ///   7   Event
    ///   8   Method
    ///   9   Mutex
    ///   10  Operation Region
    ///   11  Power Resource
    ///   12  Processor
    ///   13  Thermal Zone
    ///   14  Buffer Field
    ///   15  DDB Handle
    ///   16  Debug Object
    ///   >16 Reserved
    /// ```
    impl<'a> Parse<'a> for ObjectType {
        parser_fn!(parse('a i) -> Self {
            let (i, b) = num::le_u8(i)?;
            let value = match b {
                0  => Self::Uninitialized,
                1  => Self::Integer,
                2  => Self::String,
                3  => Self::Buffer,
                4  => Self::Package,
                5  => Self::FieldUnit,
                6  => Self::Device,
                7  => Self::Event,
                8  => Self::Method,
                9  => Self::Mutex,
                10 => Self::OperationRegion,
                11 => Self::PowerResource,
                12 => Self::Processor,
                13 => Self::ThermalZone,
                14 => Self::BufferField,
                15 => Self::DDBHandle,
                16 => Self::DebugObject,
                _ => return err(i, ErrorKind::Tag),
            };
            Ok((i, value))
        });
    }
}


/// Miscellaneous objects, defined in §20.2.6
pub mod misc {
    use super::super::misc::*;
    use super::util::*;
    use super::*;

    /// Grammar:
    ///
    /// ```text
    /// ArgObj := Arg0Op | Arg1Op | Arg2Op | Arg3Op | Arg4Op | Arg5Op | Arg6Op
    /// Arg0Op := 0x68
    /// Arg1Op := 0x69
    /// Arg2Op := 0x6A
    /// Arg3Op := 0x6B
    /// Arg4Op := 0x6C
    /// Arg5Op := 0x6D
    /// Arg6Op := 0x6E
    /// ```
    impl<'a> Parse<'a> for ArgObject {
        parser_fn! {
            parse<'a> -> Self = alt((
                value(Self::Arg0, tag_byte(0x68)),
                value(Self::Arg1, tag_byte(0x69)),
                value(Self::Arg2, tag_byte(0x6a)),
                value(Self::Arg3, tag_byte(0x6b)),
                value(Self::Arg4, tag_byte(0x6c)),
                value(Self::Arg5, tag_byte(0x6d)),
                value(Self::Arg6, tag_byte(0x6e)),
            ))
        }
    }


    /// Grammar:
    ///
    /// ```text
    /// LocalObj := Local0Op | Local1Op | Local2Op | Local3Op | Local4Op | Local5Op |
    ///             Local6Op | Local7Op
    /// Local0Op := 0x60
    /// Local1Op := 0x61
    /// Local2Op := 0x62
    /// Local3Op := 0x63
    /// Local4Op := 0x64
    /// Local5Op := 0x65
    /// Local6Op := 0x66
    /// Local7Op := 0x67
    /// ```
    impl<'a> Parse<'a> for LocalObject {
        parser_fn! {
            parse<'a> -> Self = alt((
                value(Self::Local0, tag_byte(0x60)),
                value(Self::Local1, tag_byte(0x61)),
                value(Self::Local2, tag_byte(0x62)),
                value(Self::Local3, tag_byte(0x63)),
                value(Self::Local4, tag_byte(0x64)),
                value(Self::Local5, tag_byte(0x65)),
                value(Self::Local6, tag_byte(0x66)),
                value(Self::Local7, tag_byte(0x67)),
            ))
        }
    }


    /// Grammar:
    ///
    /// ```text
    /// DebugObj := DebugOp
    /// DebugOp  := ExtOpPrefix 0x31
    /// ```
    impl<'a> Parse<'a> for DebugObject {
        parser_fn! {
            parse<'a> -> Self =
                ext_op(value(DebugObject, tag_byte(0x31)))
        }
    }
}
