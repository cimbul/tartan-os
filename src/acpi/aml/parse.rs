//! Parsers for AML bytecode.
//!
//! AML bytecode grammar is defined in §20.2. The top-level production is:
//!
//! ```text
//! AMLCode := DefBlockHeader TermList
//! ```
//!
//! Everything in `DefBlockHeader` just repeats the fixed
//! [`DescriptionHeader`](crate::acpi::DescriptionHeader) struct used in other tables,
//! so there's no reason to write a parser for it.

#![allow(clippy::wildcard_imports)]

use alloc::vec::Vec;
use alloc::vec;
use core::convert::TryFrom;
use nom::IResult;
use nom::branch::alt;
use nom::bytes::complete as bytes;
use nom::combinator::{all_consuming, flat_map, map, opt, rest, value, verify};
use nom::error::{ErrorKind, ParseError};
use nom::number::complete as num;
use nom::multi;
use nom::sequence::{preceded, tuple};


/// An object that can be parsed from AML bytecode
trait Parse<'a> where Self: Sized {
    fn parse<E: ParseError<&'a [u8]>>(i: &'a [u8]) -> IResult<&[u8], Self, E>;
}



#[macro_use]
mod util {
    use super::*;

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
                    let parser = $imp;
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
            $vis fn $name<'a, E: ::nom::error::ParseError<&'a [u8]>>(
                $input: &'a [u8]
            ) -> ::nom::IResult<&[u8], $ret_typ, E> $imp
        };

        // Use lifetime from scope
        [
            $( #[$meta:meta] )*
            $vis:vis $name:ident ($a:lifetime $input:ident)
                -> $ret_typ:ty
            $imp:block
        ] => {
            $(#[$meta])*
            $vis fn $name<E: ::nom::error::ParseError<&$a [u8]>>(
                $input: &$a [u8]
            ) -> ::nom::IResult<&[u8], $ret_typ, E> $imp
        };
    }

    /// Parse struct fields in order using an initializer-like syntax
    ///
    /// # Example
    ///
    /// ```
    /// # #[macro_use] extern crate tartan_os;
    /// # use nom::IResult;
    /// # use nom::character::complete::alphanumeric1;
    /// # use nom::number::complete::le_u16;
    /// #
    /// type Parser<'a, T> = fn(&'a [u8]) -> IResult<&'a [u8], T, ()>;
    ///
    /// # #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    /// struct Foo<'a> { a: u16, b: &'a [u8] };
    ///
    /// // Will parse `a`, then `b`, and return the struct if both succeed
    /// let parse_foo: Parser<Foo> = struct_parser!(
    ///     Foo {
    ///         a: le_u16,
    ///         b: alphanumeric1,
    ///     }
    /// );
    ///
    /// assert_eq!(
    ///     parse_foo(b"\x34\x12Bar10"),
    ///     Ok((b"" as &[u8], Foo { a: 0x1234, b: b"Bar10" }))
    /// );
    /// ```
    #[macro_export]
    macro_rules! struct_parser {
        // Struct form
        [
            $struct:ident $( :: $struct_x:ident )* {
                $( $field:ident : $parser:expr ),+
                $(,)?
            }
        ] => {
            |i| {
                $( let (i, $field) = $parser(i)?; )+
                let result = ($struct $(::$struct_x)* { $($field),+ });
                Ok((i, result))
            }
        };

        // Tuple form
        [
            $struct:ident $( :: $struct_x:ident )* (
                $( $parser:expr ),+
                $(,)?
            )
        ] => {
            |i| {
                let (i, t) = nom::sequence::tuple(( $($parser),+ ))(i)?;
                let result = Fn::call(& $struct $(::$struct_x)*, t);
                Ok((i, result))
            }
        };
    }

    #[cfg(test)]
    #[macro_export]
    macro_rules! assert_parses {
        ($parser:expr, $input:expr, $rest:expr, $output:expr $(,)?) => {
            let parser = $parser;
            let input = $input;
            let expected_rest = $rest;
            let expected_output = $output;
            let result: nom::IResult<_, _, ()> = parser(input);
            match result {
                Err(_) => panic!(
                    "\nInput could not be parsed\n   input: {:?}\n  wanted: {:?}\n  parser: {}\n",
                    input,
                    expected_output,
                    stringify!($parser),
                ),
                Ok((actual_rest, actual_output)) => {
                    assert!(
                        actual_output == expected_output,
                        "\nDid not get expected output from parser\n  wanted: {:?}\n     got: {:?}\n   input: {:?}\n    rest: {:?}\n  parser: {}\n",
                        expected_output,
                        actual_output,
                        input,
                        actual_rest,
                        stringify!($parser),
                    );
                    assert!(
                        actual_rest == expected_rest,
                        "\nParser did not consume expected data\n  wanted: {:?}\n     got: {:?}\n   input: {:?}\n  output: {:?}\n  parser: {}\n",
                        expected_rest,
                        actual_rest,
                        input,
                        actual_output,
                        stringify!($parser),
                    );
                }
            }
        };
    }

    #[cfg(test)]
    #[macro_export]
    macro_rules! assert_errors {
        ($parser:expr, $input:expr) => {
            let parser = $parser;
            let input = $input;
            let result: nom::IResult<_, _, ()> = parser(input);
            if let Ok((_rest, output)) = result {
                panic!(
                    "\nExpected error from parser, but got output\n   input: {:?}\n  output: {:?}\n  parser: {}\n",
                    input,
                    output,
                    stringify!($parser),
                );
            }
        }
    }

    /// Shortcut for constructing parser errors
    pub fn err<I, O, E: ParseError<I>>(i: I, kind: ErrorKind) -> IResult<I, O, E> {
        Err(nom::Err::Error(E::from_error_kind(i, kind)))
    }

    /// Recognizes a single byte. Possibly slightly more efficient than `bytes::tag()`
    /// with a one-byte string, but I didn't actually profile it.
    pub fn tag_byte<'a, E: ParseError<&'a [u8]>>(
        b: u8,
    ) -> impl Fn(&'a [u8]) -> IResult<&[u8], u8, E> {
        move |i: &[u8]| {
            if i.is_empty() {
                err(i, ErrorKind::Tag)
            } else if i[0] == b {
                Ok((&i[1..], b))
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
        p: P
    ) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], O, E>
    where
        P: Fn(&'a [u8]) -> IResult<&'a [u8], O, E>,
        E: ParseError<&'a [u8]>
    {
        preceded(tag_byte(0x5b), p)
    }

    parser_fn! {
        /// Recognizes a null-terminated (C-style), possibly-empty 7-bit ASCII string.
        /// Strips the null terminator.
        pub c_ascii_str(i) -> &str {
            let (i, str_bytes) = bytes::take_till(|b: u8| b == 0 || !b.is_ascii())(i)?;
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

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn test_tag_byte() {
            let tag_ab = tag_byte(0xab);
            assert_errors!(&tag_ab, &[]);
            assert_errors!(&tag_ab, &[0x12]);
            assert_errors!(&tag_ab, &[0x0f]);
            assert_parses!(&tag_ab, &[0xab],       &[],     0xab);
            assert_parses!(&tag_ab, &[0xab, 0xcd], &[0xcd], 0xab);

            let tag_0f = tag_byte(0x0f);
            assert_errors!(&tag_0f, &[]);
            assert_errors!(&tag_0f, &[0x12]);
            assert_errors!(&tag_0f, &[0xab]);
            assert_parses!(&tag_0f, &[0x0f],       &[],     0x0f);
            assert_parses!(&tag_0f, &[0x0f, 0x9a], &[0x9a], 0x0f);
        }

        #[test]
        fn test_ext_op() {
            let ext_xyz = ext_op(bytes::is_a("XYZ"));
            assert_errors!(&ext_xyz, b"");
            assert_errors!(&ext_xyz, b"\xd8");
            assert_errors!(&ext_xyz, b"\x5b");
            assert_errors!(&ext_xyz, b"\xd8ZZZ");
            assert_parses!(&ext_xyz, b"\x5bZZZ",  b"",  b"ZZZ");
            assert_parses!(&ext_xyz, b"\x5bZZZA", b"A", b"ZZZ");
            assert_errors!(&ext_xyz, b"\x5bAZZZ");
        }

        #[test]
        fn test_c_ascii_str() {
            // Empty
            assert_errors!(c_ascii_str, b"");
            assert_parses!(c_ascii_str, b"\0",     b"",     "");
            assert_parses!(c_ascii_str, b"\0z",    b"z",    "");
            assert_parses!(c_ascii_str, b"\0\x80", b"\x80", "");

            // One-char
            assert_errors!(c_ascii_str, b"w");
            assert_parses!(c_ascii_str, b"w\0",     b"",     "w");
            assert_parses!(c_ascii_str, b" \0",     b"",     " ");
            assert_parses!(c_ascii_str, b"*\0",     b"",     "*");
            assert_parses!(c_ascii_str, b"*\0*",    b"*",    "*");
            assert_parses!(c_ascii_str, b"*\0\xff", b"\xff", "*");
            assert_parses!(c_ascii_str, b"\x0c\0",  b"",     "\x0c");
            assert_parses!(c_ascii_str, b"\x7f\0",  b"",     "\x7f");
            assert_errors!(c_ascii_str, b"\x80\0");
            assert_errors!(c_ascii_str, b"\xff\0");

            // N-char
            assert_errors!(c_ascii_str, b"j8Av0#\x14*)%#!n");
            assert_parses!(c_ascii_str, b"j8Av0#\x14*)%#!n\0", b"",  "j8Av0#\x14*)%#!n");
            assert_parses!(c_ascii_str, b"+\x01&a9\x7f\0",     b"",  "+\x01&a9\x7f");
            assert_parses!(c_ascii_str, b"+\x01&a9\x7f\0z",    b"z", "+\x01&a9\x7f");
            assert_errors!(c_ascii_str, b"+\x01&a9\x80\0");
            assert_errors!(c_ascii_str, b"+\xff&a9\x7f\0");
        }
    }
}


/// Name objects, defined in §20.2.2
pub mod name {
    use alloc::boxed::Box;
    use super::*;
    use super::util::*;
    use super::super::name::*;
    use super::super::misc::*;
    use super::super::term::ReferenceExpressionOpcode;

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
                b >= b'A' && b <= b'Z' || b == b'_'
            }

            const fn is_name_char(b: u8) -> bool {
                is_lead_name_char(b) || b >= b'0' && b <= b'9'
            }

            const fn is_name_seg(n: &[u8]) -> bool {
                n.len() >= 3
                    && is_lead_name_char(n[0])
                    && is_name_char(n[1])
                    && is_name_char(n[2])
                    && is_name_char(n[3])
            }

            let (i, n) = verify(bytes::take(4_usize), is_name_seg)(i)?;
            Ok((i, NameSeg([n[0], n[1], n[2], n[3]])))
        });
    }

    #[cfg(test)]
    mod test_name_seg {
        use super::*;

        #[test]
        fn test_parse() {
            // Exactly 4 characters
            assert_errors!(NameSeg::parse, b"");
            assert_errors!(NameSeg::parse, b"A");
            assert_errors!(NameSeg::parse, b"A1");
            assert_errors!(NameSeg::parse, b"A12");
            assert_parses!(NameSeg::parse, b"A123",   b"",   NameSeg(*b"A123"));
            assert_parses!(NameSeg::parse, b"A1234",  b"4",  NameSeg(*b"A123"));
            assert_parses!(NameSeg::parse, b"A12345", b"45", NameSeg(*b"A123"));

            // Starts with uppercase letter or underscore
            assert_parses!(NameSeg::parse, b"G123", b"", NameSeg(*b"G123"));
            assert_parses!(NameSeg::parse, b"Z123", b"", NameSeg(*b"Z123"));
            assert_parses!(NameSeg::parse, b"_123", b"", NameSeg(*b"_123"));

            // Does not start with lowercase letter, digit, or any other byte
            assert_errors!(NameSeg::parse, b"a123");
            assert_errors!(NameSeg::parse, b"j123");
            assert_errors!(NameSeg::parse, b"z123");
            assert_errors!(NameSeg::parse, b"0123");
            assert_errors!(NameSeg::parse, b"1123");
            assert_errors!(NameSeg::parse, b"9123");
            assert_errors!(NameSeg::parse, b"@123");
            assert_errors!(NameSeg::parse, b"[123");
            assert_errors!(NameSeg::parse, b" 123");
            assert_errors!(NameSeg::parse, b"\0123");
            assert_errors!(NameSeg::parse, b"\x1e123");
            assert_errors!(NameSeg::parse, b"\x7f123");
            assert_errors!(NameSeg::parse, b"\xff123");

            // Rest are uppercase letters, underscores, or digits
            assert_parses!(NameSeg::parse, b"XAAA", b"", NameSeg(*b"XAAA"));
            assert_parses!(NameSeg::parse, b"XZZZ", b"", NameSeg(*b"XZZZ"));
            assert_parses!(NameSeg::parse, b"X000", b"", NameSeg(*b"X000"));
            assert_parses!(NameSeg::parse, b"X999", b"", NameSeg(*b"X999"));
            assert_parses!(NameSeg::parse, b"X___", b"", NameSeg(*b"X___"));
            assert_parses!(NameSeg::parse, b"GZ4B", b"", NameSeg(*b"GZ4B"));
            assert_parses!(NameSeg::parse, b"N_X8", b"", NameSeg(*b"N_X8"));
            assert_parses!(NameSeg::parse, b"R6F_", b"", NameSeg(*b"R6F_"));

            // Rest are not lowercase letters
            assert_errors!(NameSeg::parse, b"XaAA");
            assert_errors!(NameSeg::parse, b"XAaA");
            assert_errors!(NameSeg::parse, b"XAAa");
            assert_errors!(NameSeg::parse, b"IzFY");
            assert_errors!(NameSeg::parse, b"TJBr");

            // Rest are not any other byte
            assert_errors!(NameSeg::parse, b"X@AA");
            assert_errors!(NameSeg::parse, b"XA[A");
            assert_errors!(NameSeg::parse, b"XAA/");
            assert_errors!(NameSeg::parse, b"X:AA");
            assert_errors!(NameSeg::parse, b"XA\0A");
            assert_errors!(NameSeg::parse, b"XAA ");
            assert_errors!(NameSeg::parse, b"XAA\0");
            assert_errors!(NameSeg::parse, b"X\x18AA");
            assert_errors!(NameSeg::parse, b"XA\xffA");
        }
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
        dual_name -> Vec<NameSeg> = preceded(
            tag_byte(0x2e),
            multi::count(NameSeg::parse, 2),
        )
    }

    parser_fn! {
        /// See grammar for [`NameString`]
        multi_name -> Vec<NameSeg> = preceded(
            tag_byte(0x2f),
            flat_map(num::le_u8, |n| multi::count(NameSeg::parse, n.into())),
        )
    }

    #[cfg(test)]
    mod test_name_string {
        use super::*;
        use super::NameString as NS;

        #[test]
        fn test_parse_errors() {
            // No prefix for 0 or 2+ names, and not valid single name
            assert_errors!(NS::parse, b"");
            assert_errors!(NS::parse, b"A");
            assert_errors!(NS::parse, b"A1");
            assert_errors!(NS::parse, b"A12");

            // Unrecognized prefixes
            assert_errors!(NS::parse, b"\x2dA123");
            assert_errors!(NS::parse, b"&A123");
            assert_errors!(NS::parse, b"\\9A123");

            // Anchor without any name
            assert_errors!(NS::parse, b"\\");
            assert_errors!(NS::parse, b"^");
        }

        #[test]
        fn test_parse_null() {
            assert_parses!(NS::parse, b"\0",   b"",  NS::new(&[]));
            assert_parses!(NS::parse, b"\0K",  b"K", NS::new(&[]));
            assert_parses!(NS::parse, b"^^\0", b"",  NS::new_parent(2, &[]));
            assert_parses!(NS::parse, b"\\\0", b"",  NS::new_root(&[]));
        }

        #[test]
        fn test_parse_single() {
            let ns = &[NameSeg(*b"A123")];
            assert_parses!(NS::parse, b"A123",    b"",  NS::new(ns));
            assert_parses!(NS::parse, b"A1234",   b"4", NS::new(ns));
            assert_parses!(NS::parse, b"^^^A123", b"",  NS::new_parent(3, ns));
            assert_parses!(NS::parse, b"\\A123",  b"",  NS::new_root(ns));
        }

        #[test]
        fn test_parse_dual() {
            let ns = &[NameSeg(*b"A___"), NameSeg(*b"B___")];
            assert_parses!(NS::parse, b"\x2eA___B___",     b"",     NS::new(ns));
            assert_parses!(NS::parse, b"\x2eA___B___C",    b"C",    NS::new(ns));
            assert_parses!(NS::parse, b"\x2eA___B___C___", b"C___", NS::new(ns));
            assert_parses!(NS::parse, b"\\\x2eA___B___",   b"",     NS::new_root(ns));
            assert_parses!(NS::parse, b"^^^\x2eA___B___",  b"",     NS::new_parent(3, ns));

            assert_errors!(NS::parse, b"\x2eA___");
            assert_errors!(NS::parse, b"\x2eA___B");
            assert_errors!(NS::parse, b"\x2eA___B_");
            assert_errors!(NS::parse, b"\x2eA___B__");
        }

        #[test]
        fn test_parse_multi() {
            let a = NameSeg(*b"A___");
            let b = NameSeg(*b"B___");
            let c = NameSeg(*b"C___");
            let d = NameSeg(*b"D___");

            // Count = 0
            assert_parses!(NS::parse, b"\x2f\x00",     b"",     NS::new(&[]));
            assert_parses!(NS::parse, b"\x2f\x00A___", b"A___", NS::new(&[]));
            assert_parses!(NS::parse, b"^^\x2f\x00",   b"",     NS::new_parent(2, &[]));
            assert_parses!(NS::parse, b"\\\x2f\x00",   b"",     NS::new_root(&[]));

            // Count = 1
            assert_errors!(NS::parse, b"\x2f\x01");
            assert_errors!(NS::parse, b"\x2f\x01A");
            assert_errors!(NS::parse, b"\x2f\x01A_");
            assert_errors!(NS::parse, b"\x2f\x01A__");
            assert_parses!(NS::parse, b"\x2f\x01A___",     b"",     NS::from(a));
            assert_parses!(NS::parse, b"\x2f\x01A___",     b"",     NS::from(a));
            assert_parses!(NS::parse, b"\x2f\x01A___B",    b"B",    NS::from(a));
            assert_parses!(NS::parse, b"\x2f\x01A___B___", b"B___", NS::from(a));
            assert_parses!(NS::parse, b"\x2f\x01A___",     b"",     NS::from(a));

            // Count = 4
            assert_errors!(NS::parse, b"\x2f\x04");
            assert_errors!(NS::parse, b"\x2f\x04A___B___C___D__");
            assert_parses!(NS::parse, b"\x2f\x04A___B___C___D___",     b"",     NS::new(&[a, b, c, d]));
            assert_parses!(NS::parse, b"\x2f\x04A___B___C___D___E",    b"E",    NS::new(&[a, b, c, d]));
            assert_parses!(NS::parse, b"\x2f\x04A___B___C___D___E___", b"E___", NS::new(&[a, b, c, d]));
            assert_parses!(NS::parse, b"\\\x2f\x04A___B___C___D___",   b"",     NS::new_root(&[a, b, c, d]));
            assert_parses!(NS::parse, b"^\x2f\x04A___B___C___D___",    b"",     NS::new_parent(1, &[a, b, c, d]));
        }
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

    #[cfg(test)]
    mod test_path_anchor {
        use super::*;

        #[test]
        fn test_parse_path_anchor() {
            assert_parses!(PathAnchor::parse, b"\\",   b"",   PathAnchor::Root);
            assert_parses!(PathAnchor::parse, b"\\\\", b"\\", PathAnchor::Root);
            assert_parses!(PathAnchor::parse, b"\\A",  b"A",  PathAnchor::Root);

            assert_parses!(PathAnchor::parse, b"^",     b"",    PathAnchor::Parent(1));
            assert_parses!(PathAnchor::parse, b"^^",    b"",    PathAnchor::Parent(2));
            assert_parses!(PathAnchor::parse, b"^^%3^", b"%3^", PathAnchor::Parent(2));
            assert_parses!(PathAnchor::parse, b"^^^",   b"",    PathAnchor::Parent(3));
            assert_parses!(PathAnchor::parse,
                b"^^^^^^^^^^^^^^^^^^^^^^^^^^",
                b"",
                PathAnchor::Parent(26),
            );

            // Succeeds even if it consumes nothing
            assert_parses!(PathAnchor::parse, b"",      b"",       PathAnchor::Parent(0));
            assert_parses!(PathAnchor::parse, b"M",     b"M",      PathAnchor::Parent(0));
            assert_parses!(PathAnchor::parse, b"\x1b^", b"\x1b^",  PathAnchor::Parent(0));
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
    impl<'a> Parse<'a> for SuperName<'a> {
        parser_fn! {
            parse<'a> -> Self = alt((
                map(SimpleName::parse, Self::Name),
                value(Self::Debug, DebugObject::parse),
                map(ReferenceExpressionOpcode::parse, |r| Self::Reference(Box::new(r))),
            ))
        }
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
    use super::*;
    use super::util::*;
    use super::package::in_package;
    use super::super::data::*;
    use super::super::name::NameString;
    use super::super::term::TermArg;

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
                map(preceded(tag_byte(0x0a), num::le_u8),  Self::Byte),
                map(preceded(tag_byte(0x0b), num::le_u16), Self::Word),
                map(preceded(tag_byte(0x0c), num::le_u32), Self::DWord),
                // NOTE: These two are out of order, but this is how the spec lists it
                map(preceded(tag_byte(0x0e), num::le_u64), Self::QWord),
                map(preceded(tag_byte(0x0d), c_ascii_str), Self::String),
                value(Self::Zero, tag_byte(0x00)),
                value(Self::One, tag_byte(0x01)),
                value(Self::Ones, tag_byte(0xff)),
                value(Self::Revision, ext_op(tag_byte(0x30))),
                map(Buffer::parse, Self::Buffer),
            ))
        }
    }

    #[cfg(test)]
    mod test_computational_data {
        use super::*;
        use super::ComputationalData as CD;
        use super::super::super::misc::LocalObject;

        #[test]
        fn test_parse_byte() {
            assert_errors!(CD::parse, b"\x0a");
            assert_parses!(CD::parse, b"\x0a\xf3",     b"",     CD::Byte(0xf3));
            assert_parses!(CD::parse, b"\x0a\xf3\x86", b"\x86", CD::Byte(0xf3));
        }

        #[test]
        fn test_parse_word() {
            assert_errors!(CD::parse, b"\x0b");
            assert_errors!(CD::parse, b"\x0b\x5c");
            assert_parses!(CD::parse, b"\x0b\x5c\xf3",     b"",     CD::Word(0xf35c));
            assert_parses!(CD::parse, b"\x0b\x5c\xf3\x86", b"\x86", CD::Word(0xf35c));
        }

        #[test]
        fn test_parse_dword() {
            assert_errors!(CD::parse, b"\x0c");
            assert_errors!(CD::parse, b"\x0c\x8b");
            assert_errors!(CD::parse, b"\x0c\x8b\xd9");
            assert_errors!(CD::parse, b"\x0c\x8b\xd9\x5c");
            assert_parses!(CD::parse, b"\x0c\x8b\xd9\x5c\xf3",     b"",     CD::DWord(0xf35c_d98b));
            assert_parses!(CD::parse, b"\x0c\x8b\xd9\x5c\xf3\x86", b"\x86", CD::DWord(0xf35c_d98b));
        }

        #[test]
        fn test_parse_qword() {
            assert_errors!(CD::parse, b"\x0e");
            assert_errors!(CD::parse, b"\x0e\x36");
            assert_errors!(CD::parse, b"\x0e\x36\xe2\x73\x2e");
            assert_errors!(CD::parse, b"\x0e\x36\xe2\x73\x2e\x8b\xd9\x5c");
            assert_parses!(CD::parse, b"\x0e\x36\xe2\x73\x2e\x8b\xd9\x5c\xf3",     b"",     CD::QWord(0xf35c_d98b_2e73_e236));
            assert_parses!(CD::parse, b"\x0e\x36\xe2\x73\x2e\x8b\xd9\x5c\xf3\x86", b"\x86", CD::QWord(0xf35c_d98b_2e73_e236));
        }

        #[test]
        fn test_parse_string() {
            assert_errors!(CD::parse, b"\x0d");
            assert_errors!(CD::parse, b"\x0dH");
            assert_parses!(CD::parse, b"\x0d\0",                  b"",   CD::String(""));
            assert_parses!(CD::parse, b"\x0dH\0",                 b"",   CD::String("H"));
            assert_parses!(CD::parse, b"\x0dHello, world!\0",     b"",   CD::String("Hello, world!"));
            assert_parses!(CD::parse, b"\x0dHello, world!\0XX",   b"XX", CD::String("Hello, world!"));
            assert_parses!(CD::parse, b"\x0dHe&&0,\nw_\x1eld!\0", b"",   CD::String("He&&0,\nw_\x1eld!"));
            assert_errors!(CD::parse, b"\x0dHello, world!");
            assert_errors!(CD::parse, b"\x0dHello,\x80world!\0");
        }

        #[test]
        fn test_constants() {
            assert_errors!(CD::parse, b"");

            assert_parses!(CD::parse, b"\x00",     b"",     CD::Zero);
            assert_parses!(CD::parse, b"\x01",     b"",     CD::One);
            assert_parses!(CD::parse, b"\xff",     b"",     CD::Ones);
            assert_parses!(CD::parse, b"\xff\x9e", b"\x9e", CD::Ones);

            assert_errors!(CD::parse, b"\x5b");
            assert_parses!(CD::parse, b"\x5b\x30", b"", CD::Revision);
        }

        #[test]
        fn test_buffer() {
            assert_errors!(CD::parse, b"\x11");
            assert_errors!(CD::parse, b"\x11\x00");
            assert_errors!(CD::parse, b"\x11\x00\x00");

            // Single-byte package length, single-byte size, no initializer
            assert_parses!(CD::parse, b"\x11\x01\x00", b"", CD::Buffer(Buffer {
                size: CD::Zero.into(),
                initializer: &[],
            }));
            // Single-byte initializer
            assert_parses!(CD::parse, b"\x11\x02\x00\x00", b"", CD::Buffer(Buffer {
                size: CD::Zero.into(),
                initializer: &[0x00],
            }));
            // Multi-byte initializer + trailing data
            assert_parses!(CD::parse, b"\x11\x03\x00\xab\xcd\xef", b"\xef", CD::Buffer(Buffer {
                size: CD::Zero.into(),
                initializer: &[0xab, 0xcd],
            }));
            // Multi-byte size term
            assert_parses!(CD::parse, b"\x11\x03\x0b\x0a\x00", b"", CD::Buffer(Buffer {
                size: CD::Word(10).into(),
                initializer: &[],
            }));
            // Single-byte size term
            assert_parses!(CD::parse, b"\x11\x03\x62\x1f\xdb", b"", CD::Buffer(Buffer {
                size: LocalObject::Local2.into(),
                initializer: &[0x1f, 0xdb],
            }));
            // Multi-byte package length
            assert_parses!(CD::parse, b"\x11\x46\x00\x62\x12\x34\x56\x78\x9a", b"", CD::Buffer(Buffer {
                size: LocalObject::Local2.into(),
                initializer: &[0x12, 0x34, 0x56, 0x78, 0x9a],
            }));

            // Not enough data for package length
            assert_errors!(CD::parse, b"\x11\x46\x00\x62\x12\x34\x56\x78");

            // Invalid term
            assert_errors!(CD::parse, b"\x11\x01\x0b");
            assert_errors!(CD::parse, b"\x11\x02\x0b\x00");

            // Package length cuts off valid term
            assert_errors!(CD::parse, b"\x11\x02\x0b\x00\x01");
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
            parse<'a> -> Self = preceded(
                tag_byte(0x11),
                in_package(struct_parser! {
                    Buffer {
                        size: TermArg::parse,
                        initializer: rest,
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
            parse<'a> -> Self = preceded(
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
            parse<'a> -> Self = preceded(
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

    parser_fn! {
        /// Parse the length of a package
        ///
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
        pub parse_package_length(i) -> u32 {
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
            Ok((i, length_high | length_low))
        }
    }

    /// Execute a parser inside a length-prefixed package
    pub fn in_package<'a, P, O, E>(
        inner_parser: P,
    ) -> impl Fn(&'a [u8]) -> IResult<&[u8], O, E>
    where
        P: Fn(&'a [u8]) -> IResult<&[u8], O, E>,
        E: ParseError<&'a [u8]>
    {
        move |i: &'a [u8]| {
            let (i, package_length) = parse_package_length(i)?;
            let (i, package_data) = bytes::take(package_length)(i)?;
            let (_, parsed_package) = all_consuming(&inner_parser)(package_data)?;
            Ok((i, parsed_package))
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn test_package_length_one_byte() {
            assert_errors!(parse_package_length, &[]);

            // Values with fewer than 6 bits parse as themselves and only consume one byte
            for n in 0..0x40_u8 {
                assert_parses!(parse_package_length, &[n      ], &[],     n.into());
                assert_parses!(parse_package_length, &[n, 0xa4], &[0xa4], n.into());
            }

            // Every other value requires multiple bytes
            for n in 0x40_u8..=0xff {
                assert_errors!(parse_package_length, &[n]);
            }
        }

        #[test]
        fn test_package_length_two_byte() {
            // Legal two-byte values start with 0x4
            assert_parses!(parse_package_length, &[0x41, 0x32],       &[],     0x0321);
            assert_parses!(parse_package_length, &[0x4e, 0xd9],       &[],     0x0d9e);
            assert_parses!(parse_package_length, &[0x40, 0x01],       &[],     0x0010);
            assert_parses!(parse_package_length, &[0x40, 0x01, 0x2a], &[0x2a], 0x0010);
            assert_parses!(parse_package_length, &[0x4f, 0xff],       &[],     0x0fff);
            assert_parses!(parse_package_length, &[0x4f, 0xff, 0xa5], &[0xa5], 0x0fff);

            // Bits 5-6 are reserved and must be unset, but we should ignore them anyway
            assert_parses!(parse_package_length, &[0x4e, 0xd9],       &[],     0x0d9e);
            assert_parses!(parse_package_length, &[0x5e, 0xd9, 0x8c], &[0x8c], 0x0d9e);
            assert_parses!(parse_package_length, &[0x6e, 0xd9],       &[],     0x0d9e);
            assert_parses!(parse_package_length, &[0x7e, 0xd9, 0x8c], &[0x8c], 0x0d9e);

            // These could have fit in one byte, but we can still parse them
            assert_parses!(parse_package_length, &[0x40, 0x00],       &[],     0x0000);
            assert_parses!(parse_package_length, &[0x41, 0x00],       &[],     0x0001);
            assert_parses!(parse_package_length, &[0x41, 0x00, 0xe9], &[0xe9], 0x0001);
            assert_parses!(parse_package_length, &[0x4f, 0x00],       &[],     0x000f);
        }

        #[test]
        fn test_package_length_three_byte() {
            // Legal three-byte values start with 0x8
            assert_parses!(parse_package_length, &[0x81, 0x32, 0x54],       &[],     0x0005_4321);
            assert_parses!(parse_package_length, &[0x89, 0x7e, 0xa3],       &[],     0x000a_37e9);
            assert_parses!(parse_package_length, &[0x80, 0x00, 0x01],       &[],     0x0000_1000);
            assert_parses!(parse_package_length, &[0x80, 0x00, 0x01, 0xb7], &[0xb7], 0x0000_1000);
            assert_parses!(parse_package_length, &[0x8f, 0xff, 0xff],       &[],     0x000f_ffff);
            assert_parses!(parse_package_length, &[0x8f, 0xff, 0xff, 0x4b], &[0x4b], 0x000f_ffff);

            // Bits 5-6 are reserved and must be unset, but we should ignore them anyway
            assert_parses!(parse_package_length, &[0x89, 0x7e, 0xa3, 0x5b], &[0x5b], 0x000a_37e9);
            assert_parses!(parse_package_length, &[0x99, 0x7e, 0xa3],       &[],     0x000a_37e9);
            assert_parses!(parse_package_length, &[0xa9, 0x7e, 0xa3, 0x5b], &[0x5b], 0x000a_37e9);
            assert_parses!(parse_package_length, &[0xb9, 0x7e, 0xa3],       &[],     0x000a_37e9);

            // These could have fit in fewer bytes, but we can still parse them
            assert_parses!(parse_package_length, &[0x80, 0x00, 0x00],       &[],     0x0000);
            assert_parses!(parse_package_length, &[0x81, 0x00, 0x00],       &[],     0x0001);
            assert_parses!(parse_package_length, &[0x81, 0x00, 0x00, 0x6d], &[0x6d], 0x0001);
            assert_parses!(parse_package_length, &[0x80, 0x01, 0x00],       &[],     0x0010);
            assert_parses!(parse_package_length, &[0x8f, 0xff, 0x00],       &[],     0x0fff);
        }

        #[test]
        fn test_package_length_four_byte() {
            // Legal four-byte values start with 0xc
            assert_parses!(parse_package_length, &[0xc1, 0x32, 0x54, 0x76],       &[],     0x0765_4321);
            assert_parses!(parse_package_length, &[0xcb, 0xe3, 0xd8, 0x49],       &[],     0x049d_8e3b);
            assert_parses!(parse_package_length, &[0xc0, 0x00, 0x00, 0x01],       &[],     0x0010_0000);
            assert_parses!(parse_package_length, &[0xc0, 0x00, 0x00, 0x01, 0x2f], &[0x2f], 0x0010_0000);
            assert_parses!(parse_package_length, &[0xcf, 0xff, 0xff, 0xff],       &[],     0x0fff_ffff);
            assert_parses!(parse_package_length, &[0xcf, 0xff, 0xff, 0xff, 0xa4], &[0xa4], 0x0fff_ffff);

            // Bits 5-6 are reserved and must be unset, but we should ignore them anyway
            assert_parses!(parse_package_length, &[0xcb, 0xe3, 0xd8, 0x49, 0xbf], &[0xbf], 0x049d_8e3b);
            assert_parses!(parse_package_length, &[0xdb, 0xe3, 0xd8, 0x49],       &[],     0x049d_8e3b);
            assert_parses!(parse_package_length, &[0xeb, 0xe3, 0xd8, 0x49],       &[],     0x049d_8e3b);
            assert_parses!(parse_package_length, &[0xfb, 0xe3, 0xd8, 0x49, 0xbf], &[0xbf], 0x049d_8e3b);

            // These could have fit in fewer bytes, but we can still parse them
            assert_parses!(parse_package_length, &[0xc0, 0x00, 0x00, 0x00],       &[],     0x0000_0000);
            assert_parses!(parse_package_length, &[0xc1, 0x00, 0x00, 0x00],       &[],     0x0000_0001);
            assert_parses!(parse_package_length, &[0xc1, 0x00, 0x00, 0x00, 0xc7], &[0xc7], 0x0000_0001);
            assert_parses!(parse_package_length, &[0xc0, 0x01, 0x00, 0x00],       &[],     0x0000_0010);
            assert_parses!(parse_package_length, &[0xc0, 0x00, 0x01, 0x00],       &[],     0x0000_1000);
            assert_parses!(parse_package_length, &[0xcf, 0xff, 0xff, 0x00],       &[],     0x000f_ffff);
        }

        #[test]
        fn test_package() {
            let package_xyz = in_package(bytes::take_while(|b| b"XYZ".contains(&b)));
            assert_errors!(&package_xyz, b"");
            assert_parses!(&package_xyz, b"\x00", b"", b"");
            assert_errors!(&package_xyz, b"\x01");
            assert_errors!(&package_xyz, b"\x01A");
            assert_parses!(&package_xyz, b"\x01Z",   b"",  b"Z");
            assert_parses!(&package_xyz, b"\x01ZZ",  b"Z", b"Z");  // Can't read past end
            assert_parses!(&package_xyz, b"\x01ZA",  b"A", b"Z");
            assert_parses!(&package_xyz, b"\x02ZZ",  b"",  b"ZZ");
            assert_errors!(&package_xyz, b"\x02");
            assert_errors!(&package_xyz, b"\x02Z");
            assert_errors!(&package_xyz, b"\x02ZA");

            // Fails if inner parser requires more data
            let package_take_4 = in_package::<_, _, ()>(bytes::take(4_usize));
            assert_errors!(&package_take_4, b"\x00");
            assert_errors!(&package_take_4, b"\x01A");
            assert_errors!(&package_take_4, b"\x02AB");
            assert_errors!(&package_take_4, b"\x03ABC");
            assert_parses!(&package_take_4, b"\x04ABCD",  b"",  b"ABCD");
            assert_parses!(&package_take_4, b"\x04ABCDE", b"E", b"ABCD");
            assert_errors!(&package_take_4, b"\x05ABCDE");  // Must consume whole package
        }
    }
}


/// Terms, defined in §20.2.5
pub mod term {
    use alloc::boxed::Box;
    use super::*;
    use super::util::*;
    use super::name::{parse_target};
    use super::package::{in_package, parse_package_length};
    use super::super::term::*;
    use super::super::data::{Buffer, DataRefObject, DataObject, Package, VarPackage};
    use super::super::misc::{ArgObject, LocalObject};
    use super::super::name::{NameSeg, NameString, SimpleName, SuperName};


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
    impl<'a> Parse<'a> for TermArg<'a> {
        parser_fn! {
            parse<'a> -> Self = alt((
                map(ExpressionOpcode::parse, |x| Self::Expression(Box::new(x))),
                map(DataObject::parse,       |x| Self::Data(Box::new(x))),
                map(ArgObject::parse,            Self::Arg),
                map(LocalObject::parse,          Self::Local),
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
        parse_alias -> NameSpaceModifier<'a> = preceded(
            tag_byte(0x06),
            struct_parser!(
                NameSpaceModifier::Alias {
                    source: NameString::parse,
                    alias: NameString::parse,
                }
            )
        )
    }

    parser_fn! {
        /// Grammar:
        ///
        /// ```text
        /// DefName := NameOp NameString DataRefObject
        /// NameOp  := 0x08
        /// ```
        parse_name -> NameSpaceModifier<'a> = preceded(
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
        parse_scope -> NameSpaceModifier<'a> = preceded(
            tag_byte(0x10),
            in_package(struct_parser! {
                NameSpaceModifier::Scope(
                    NameString::parse,
                    multi::many0(TermObject::parse),
                )
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
        bank_field -> NamedObject<'a> = preceded(
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
        create_bit_field -> NamedObject<'a> = preceded(
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
        [$(#[$meta:meta])* $parser_name:ident $struct:ident $tag:literal] => {
            parser_fn! {
                $(#[$meta])*
                $parser_name -> NamedObject<'a> = preceded(
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
        create_byte_field CreateByteField 0x8c
    }

    parse_create_sized_field! {
        /// Grammar:
        ///
        /// ```text
        /// DefCreateWordField   := CreateWordFieldOp SourceBuff ByteIndex NameString
        /// CreateWordFieldOp    := 0x8B
        /// ```
        create_word_field CreateWordField 0x8b
    }

    parse_create_sized_field! {
        /// Grammar:
        ///
        /// ```text
        /// DefCreateDWordField  := CreateDWordFieldOp SourceBuff ByteIndex NameString
        /// CreateDWordFieldOp   := 0x8A
        /// ```
        create_dword_field CreateDWordField 0x8a
    }

    parse_create_sized_field! {
        /// Grammar:
        ///
        /// ```text
        /// DefCreateQWordField  := CreateQWordFieldOp SourceBuff ByteIndex NameString
        /// CreateQWordFieldOp   := 0x8F
        /// ```
        create_qword_field CreateQWordField 0x8f
    }

    parser_fn! {
        /// Grammar:
        ///
        /// ```text
        /// DefCreateField       := CreateFieldOp SourceBuff BitIndex NumBits NameString
        /// CreateFieldOp        := ExtOpPrefix 0x13
        /// NumBits              := TermArg => Integer
        /// ```
        create_field -> NamedObject<'a> = preceded(
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
        data_table_region -> NamedObject<'a> = preceded(
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
        device -> NamedObject<'a> = preceded(
            ext_op(tag_byte(0x82)),
            in_package(struct_parser! {
                NamedObject::Device {
                    name: NameString::parse,
                    body: multi::many0(TermObject::parse),
                }
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
        event -> NamedObject<'a> = preceded(
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
        external -> NamedObject<'a> = preceded(
            tag_byte(0x15),
            struct_parser! {
                NamedObject::External {
                    name: NameString::parse,
                    object_type: ObjectType::parse,
                    argument_count: verify(num::le_u8, |b| *b <= 7),
                }
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
        field -> NamedObject<'a> = preceded(
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
        index_field -> NamedObject<'a> = preceded(
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
        method -> NamedObject<'a> = preceded(
            tag_byte(0x14),
            in_package(struct_parser! {
                NamedObject::Method {
                    name: NameString::parse,
                    flags: MethodFlags::parse,
                    body: multi::many0(TermObject::parse),
                }
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
        mutex -> NamedObject<'a> = preceded(
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
        operation_region -> NamedObject<'a> = preceded(
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
        power_resource -> NamedObject<'a> = preceded(
            ext_op(tag_byte(0x84)),
            in_package(struct_parser! {
                NamedObject::PowerResource {
                    name: NameString::parse,
                    system_level: num::le_u8,
                    resource_order: num::le_u16,
                    body: multi::many0(TermObject::parse),
                }
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
        processor -> NamedObject<'a> = preceded(
            ext_op(tag_byte(0x83)),
            in_package(struct_parser! {
                NamedObject::Processor {
                    name: NameString::parse,
                    id: num::le_u8,
                    register_block_addr: num::le_u32,
                    register_block_length: num::le_u8,
                    body: multi::many0(TermObject::parse),
                }
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
        thermal_zone -> NamedObject<'a> = preceded(
            ext_op(tag_byte(0x85)),
            in_package(struct_parser! {
                NamedObject::ThermalZone {
                    name: NameString::parse,
                    body: multi::many0(TermObject::parse),
                }
            })
        )
    }

    #[cfg(test)]
    mod test_named_object {
        use super::*;
        use super::NamedObject as N;
        use super::super::super::data::ComputationalData;

        #[test]
        fn test_bank_field() {
            assert_errors!(N::parse, b"\x5b");
            assert_errors!(N::parse, b"\x5b\x87");
            assert_errors!(N::parse, b"\x5b\x87\x00");
            assert_errors!(N::parse, b"\x5b\x87\x01\x00");
            assert_errors!(N::parse, b"\x5b\x87\x02\x00\x00");
            assert_errors!(N::parse, b"\x5b\x87\x03\x00\x00\x00");

            assert_errors!(N::parse, b"\x5b\x87\x03\x00\x00\x00\x00");
            assert_errors!(N::parse, b"\x5b\x87\x05\x00\x00\x00\x00");

            assert_parses!(N::parse, b"\x5b\x87\x04\x00\x00\x00\x00", b"", N::BankField {
                region_name: NameString::new(&[]),
                bank_name: NameString::new(&[]),
                bank_value: ComputationalData::Zero.into(),
                flags: FieldFlags {
                    access_type: AccessType::Any,
                    lock: false,
                    update_rule: UpdateRule::Preserve
                },
                elements: vec![]
            });

            assert_parses!(N::parse, b"\x5b\x87\x0aA___B___\x6d\x55", b"", N::BankField {
                region_name: b"A___".into(),
                bank_name: b"B___".into(),
                bank_value: ArgObject::Arg5.into(),
                flags: FieldFlags {
                    access_type: AccessType::Buffer,
                    lock: true,
                    update_rule: UpdateRule::WriteAsZeros
                },
                elements: vec![]
            });

            assert_parses!(N::parse,
                //            rname
                //            |   bname   flags
                //        pkg |   |   bval|   element[0]          element[1]  rest
                //        |   |   |   |   |   |                   |           |
                b"\x5b\x87\x12B___\x00\x01\x24Z_F2\xc2\x4d\x0e\x94\x00\x4f\xd7A___\x03",
                b"A___\x03",
                N::BankField {
                    region_name: b"B___".into(),
                    bank_name: NameString::new(&[]),
                    bank_value: ComputationalData::One.into(),
                    flags: FieldFlags {
                        access_type: AccessType::QWord,
                        lock: false,
                        update_rule: UpdateRule::WriteAsOnes,
                    },
                    elements: vec![
                        FieldElement::Named { name: b"Z_F2".into(), bit_length: 0x0940_e4d2 },
                        FieldElement::Reserved { bit_length: 0x0d7f },
                    ],
                },
            );
        }

        #[test]
        fn test_bit_field() {
            assert_errors!(N::parse, b"\x8d");
            assert_errors!(N::parse, b"\x8d\x00");
            assert_errors!(N::parse, b"\x8d\x00\x00");
            assert_parses!(N::parse, b"\x8d\x00\x00\x00", b"",
                N::CreateBitField {
                    source_buffer: ComputationalData::Zero.into(),
                    bit_index: ComputationalData::Zero.into(),
                    name: NameString::new(&[]),
                }
            );
            assert_parses!(N::parse,
                b"\x8d\x6e\x0b\x3f\xec\\\x2eA___B___C___",
                b"C___",
                N::CreateBitField {
                    source_buffer: ArgObject::Arg6.into(),
                    bit_index: ComputationalData::Word(0xec3f).into(),
                    name: NameString::new_root(&[NameSeg(*b"A___"), NameSeg(*b"B___")]),
                }
            );
        }

        #[test]
        fn test_byte_field() {
            assert_errors!(N::parse, b"\x8c");
            assert_errors!(N::parse, b"\x8c\x00");
            assert_errors!(N::parse, b"\x8c\x00\x00");
            assert_parses!(N::parse, b"\x8c\x00\x00\x00", b"",
                N::CreateByteField {
                    source_buffer: ComputationalData::Zero.into(),
                    byte_index: ComputationalData::Zero.into(),
                    name: NameString::new(&[]),
                }
            );
            assert_parses!(N::parse,
                b"\x8c\x6e\x0b\x3f\xec\\\x2eA___B___C___",
                b"C___",
                N::CreateByteField {
                    source_buffer: ArgObject::Arg6.into(),
                    byte_index: ComputationalData::Word(0xec3f).into(),
                    name: NameString::new_root(&[NameSeg(*b"A___"), NameSeg(*b"B___")]),
                }
            );
        }

        #[test]
        fn test_dword_field() {
            assert_errors!(N::parse, b"\x8a");
            assert_errors!(N::parse, b"\x8a\x00");
            assert_errors!(N::parse, b"\x8a\x00\x00");
            assert_parses!(N::parse, b"\x8a\x00\x00\x00", b"",
                N::CreateDWordField {
                    source_buffer: ComputationalData::Zero.into(),
                    byte_index: ComputationalData::Zero.into(),
                    name: NameString::new(&[]),
                }
            );
            assert_parses!(N::parse,
                b"\x8a\x6e\x0b\x3f\xec\\\x2eA___B___C___",
                b"C___",
                N::CreateDWordField {
                    source_buffer: ArgObject::Arg6.into(),
                    byte_index: ComputationalData::Word(0xec3f).into(),
                    name: NameString::new_root(&[NameSeg(*b"A___"), NameSeg(*b"B___")]),
                }
            );
        }

        #[test]
        fn test_create_field() {
            assert_errors!(N::parse, b"\x5b");
            assert_errors!(N::parse, b"\x5b\x13");
            assert_errors!(N::parse, b"\x5b\x13\x00");
            assert_errors!(N::parse, b"\x5b\x13\x00\x00");
            assert_errors!(N::parse, b"\x5b\x13\x00\x00\x00");
            assert_parses!(N::parse, b"\x5b\x13\x00\x00\x00\x00", b"",
                N::CreateField {
                    source_buffer: ComputationalData::Zero.into(),
                    bit_index: ComputationalData::Zero.into(),
                    num_bits: ComputationalData::Zero.into(),
                    name: NameString::new(&[]),
                }
            );
            assert_parses!(N::parse,
                b"\x5b\x13\x6e\x0b\x3f\xec\x0a\x4c\\\x2eA___B___C___",
                b"C___",
                N::CreateField {
                    source_buffer: ArgObject::Arg6.into(),
                    bit_index: ComputationalData::Word(0xec3f).into(),
                    num_bits: ComputationalData::Byte(0x4c).into(),
                    name: NameString::new_root(&[NameSeg(*b"A___"), NameSeg(*b"B___")]),
                }
            );
        }

        #[test]
        fn test_qword_field() {
            assert_errors!(N::parse, b"\x8f");
            assert_errors!(N::parse, b"\x8f\x00");
            assert_errors!(N::parse, b"\x8f\x00\x00");
            assert_parses!(N::parse, b"\x8f\x00\x00\x00", b"",
                N::CreateQWordField {
                    source_buffer: ComputationalData::Zero.into(),
                    byte_index: ComputationalData::Zero.into(),
                    name: NameString::new(&[]),
                }
            );
            assert_parses!(N::parse,
                b"\x8f\x6e\x0b\x3f\xec\\\x2eA___B___C___",
                b"C___",
                N::CreateQWordField {
                    source_buffer: ArgObject::Arg6.into(),
                    byte_index: ComputationalData::Word(0xec3f).into(),
                    name: NameString::new_root(&[NameSeg(*b"A___"), NameSeg(*b"B___")]),
                }
            );
        }

        #[test]
        fn test_word_field() {
            assert_errors!(N::parse, b"\x8b");
            assert_errors!(N::parse, b"\x8b\x00");
            assert_errors!(N::parse, b"\x8b\x00\x00");
            assert_parses!(N::parse, b"\x8b\x00\x00\x00", b"",
                N::CreateWordField {
                    source_buffer: ComputationalData::Zero.into(),
                    byte_index: ComputationalData::Zero.into(),
                    name: NameString::new(&[]),
                }
            );
            assert_parses!(N::parse,
                b"\x8b\x6e\x0b\x3f\xec\\\x2eA___B___C___",
                b"C___",
                N::CreateWordField {
                    source_buffer: ArgObject::Arg6.into(),
                    byte_index: ComputationalData::Word(0xec3f).into(),
                    name: NameString::new_root(&[NameSeg(*b"A___"), NameSeg(*b"B___")]),
                }
            );
        }

        #[test]
        fn test_data_region() {
            assert_errors!(N::parse, b"\x5b");
            assert_errors!(N::parse, b"\x5b\x88");
            assert_errors!(N::parse, b"\x5b\x88\x00");
            assert_errors!(N::parse, b"\x5b\x88\x00\x00");
            assert_errors!(N::parse, b"\x5b\x88\x00\x00\x00");
            assert_parses!(N::parse, b"\x5b\x88\x00\x00\x00\x00", b"",
                N::DataTableRegion {
                    name: NameString::new(&[]),
                    signature: ComputationalData::Zero.into(),
                    oem_id: ComputationalData::Zero.into(),
                    oem_table_id: ComputationalData::Zero.into(),
                }
            );
            assert_parses!(N::parse, b"\x5b\x88\\_311\x0a\x42\xff\x6a\x94", b"\x94",
                N::DataTableRegion {
                    name: NameString::new_root(&[NameSeg(*b"_311")]),
                    signature: ComputationalData::Byte(0x42).into(),
                    oem_id: ComputationalData::Ones.into(),
                    oem_table_id: ArgObject::Arg2.into(),
                }
            );
        }

        #[test]
        fn test_device() {
            assert_errors!(N::parse, b"\x5b\x82");
            assert_errors!(N::parse, b"\x5b\x82\x00");
            assert_errors!(N::parse, b"\x5b\x82\x01");
            assert_errors!(N::parse, b"\x5b\x82\x00\x00");
            assert_parses!(N::parse, b"\x5b\x82\x01\x00", b"",
                N::Device {
                    name: NameString::new(&[]),
                    body: vec![],
                }
            );
            assert_parses!(N::parse,
                //        pkg name  body[0]             body[1]                 rest
                //        |   |     |                   |                       |
                b"\x5b\x82\x14^^ABCD\x8d\x63\x0a\x42_57Z\x11\x04\x0a\x3d\xf5\x83\x62\x01",
                b"\x62\x01",
                N::Device {
                    name: NameString::new_parent(2, &[NameSeg(*b"ABCD")]),
                    body: vec![
                        N::CreateBitField {
                            source_buffer: LocalObject::Local3.into(),
                            bit_index: ComputationalData::Byte(0x42).into(),
                            name: NameString::from(b"_57Z"),
                        }.into(),
                        Buffer {
                            size: ComputationalData::Byte(0x3d).into(),
                            initializer: &[0xf5, 0x83],
                        }.into(),
                    ],
                }
            );
        }

        #[test]
        fn test_event() {
            assert_errors!(N::parse, b"\x5b");
            assert_errors!(N::parse, b"\x5b\x02");
            assert_parses!(N::parse, b"\x5b\x02\x00", b"", N::Event(NameString::new(&[])));
            assert_parses!(N::parse,
                b"\x5b\x02\\\x2f\x03A___B___C___D___",
                b"D___",
                N::Event(
                    NameString::new_root(&[
                        NameSeg(*b"A___"),
                        NameSeg(*b"B___"),
                        NameSeg(*b"C___"),
                    ])
                )
            );
            assert_errors!(N::parse, b"\x5b\x02\\\x2f\x03A___B___");
        }

        #[test]
        fn test_external() {
            assert_errors!(N::parse, b"\x15");
            assert_errors!(N::parse, b"\x15\x00");
            assert_errors!(N::parse, b"\x15\x00\x00");
            assert_parses!(N::parse, b"\x15\x00\x00\x00", b"", N::External {
                name: NameString::new(&[]),
                object_type: ObjectType::Uninitialized,
                argument_count: 0,
            });
            assert_parses!(N::parse, b"\x15^_123\x10\x07\x94\x8b", b"\x94\x8b", N::External {
                name: NameString::new_parent(1, &[NameSeg(*b"_123")]),
                object_type: ObjectType::DebugObject,
                argument_count: 0x07,
            });

            // Argument count too high
            assert_errors!(N::parse, b"\x15\x00\x00\x08");
        }

        #[test]
        fn test_field() {
            assert_errors!(N::parse, b"\x5b");
            assert_errors!(N::parse, b"\x5b\x81");
            assert_errors!(N::parse, b"\x5b\x81\x00");
            assert_errors!(N::parse, b"\x5b\x81\x02");
            assert_errors!(N::parse, b"\x5b\x81\x01\x00");
            assert_errors!(N::parse, b"\x5b\x81\x02\x00");
            assert_parses!(N::parse, b"\x5b\x81\x02\x00\x00", b"", N::Field {
                region_name: NameString::new(&[]),
                flags: FieldFlags {
                    access_type: AccessType::Any,
                    lock: false,
                    update_rule: UpdateRule::Preserve,
                },
                elements: vec![],
            });
            assert_parses!(N::parse,
                //                  flags
                //        pkg rname |   element[0]          element[1]  rest
                //        |   |     |   |                   |           |
                b"\x5b\x81\x11\\ABCD\x35\x00\xc3\x85\xfd\x98_B34\x4d\xa8\x11\x15",
                b"\x11\x15",
                N::Field {
                    region_name: NameString::new_root(&[NameSeg(*b"ABCD")]),
                    flags: FieldFlags {
                        access_type: AccessType::Buffer,
                        lock: true,
                        update_rule: UpdateRule::WriteAsOnes,
                    },
                    elements: vec![
                        FieldElement::Reserved { bit_length: 0x098f_d853 },
                        FieldElement::Named {
                            name: NameSeg(*b"_B34"),
                            bit_length: 0x0a8d
                        }
                    ],
                }
            );
        }

        #[test]
        fn test_index_field() {
            assert_errors!(N::parse, b"\x5b");
            assert_errors!(N::parse, b"\x5b\x86");
            assert_errors!(N::parse, b"\x5b\x86\x00");
            assert_errors!(N::parse, b"\x5b\x86\x03");
            assert_errors!(N::parse, b"\x5b\x86\x01\x00");
            assert_errors!(N::parse, b"\x5b\x86\x03\x00");
            assert_errors!(N::parse, b"\x5b\x86\x02\x00\x00");
            assert_errors!(N::parse, b"\x5b\x86\x03\x00\x00");
            assert_parses!(N::parse, b"\x5b\x86\x03\x00\x00\x00", b"", N::IndexField {
                index_name: NameString::new(&[]),
                data_name: NameString::new(&[]),
                flags: FieldFlags {
                    access_type: AccessType::Any,
                    lock: false,
                    update_rule: UpdateRule::Preserve,
                },
                elements: vec![],
            });
            assert_parses!(N::parse,
                //            iname      flags
                //        pkg |    dname |   element[0]          element[1]  rest
                //        |   |    |     |   |                   |           |
                b"\x5b\x86\x16^_987\\ABCD\x35\x00\xc3\x85\xfd\x98_B34\x4d\xa8\x11\x15",
                b"\x11\x15",
                N::IndexField {
                    index_name: NameString::new_parent(1, &[NameSeg(*b"_987")]),
                    data_name: NameString::new_root(&[NameSeg(*b"ABCD")]),
                    flags: FieldFlags {
                        access_type: AccessType::Buffer,
                        lock: true,
                        update_rule: UpdateRule::WriteAsOnes,
                    },
                    elements: vec![
                        FieldElement::Reserved { bit_length: 0x098f_d853 },
                        FieldElement::Named {
                            name: NameSeg(*b"_B34"),
                            bit_length: 0x0a8d
                        }
                    ],
                }
            );
        }

        #[test]
        fn test_method() {
            assert_errors!(N::parse, b"\x14");
            assert_errors!(N::parse, b"\x14\x00");
            assert_errors!(N::parse, b"\x14\x01\x00");
            assert_errors!(N::parse, b"\x14\x02\x00");
            assert_parses!(N::parse, b"\x14\x02\x00\x00", b"",
                N::Method {
                    name: NameString::new(&[]),
                    flags: MethodFlags {
                        arg_count: 0,
                        serialized: false,
                        sync_level: 0,
                    },
                    body: vec![],
                }
            );
            assert_parses!(N::parse,
                //              flags
                //    pkg name  |   body[0]             body[1]                 rest
                //    |   |     |   |                   |                       |
                b"\x14\x15^^Z49F\xff\x8d\x63\x0a\x42_57Z\x11\x04\x0a\x3d\xf5\x83\x62\x01",
                b"\x62\x01",
                N::Method {
                    name: NameString::new_parent(2, &[NameSeg(*b"Z49F")]),
                    flags: MethodFlags {
                        arg_count: 0x7,
                        serialized: true,
                        sync_level: 0xf,
                    },
                    body: vec![
                        N::CreateBitField {
                            source_buffer: LocalObject::Local3.into(),
                            bit_index: ComputationalData::Byte(0x42).into(),
                            name: NameString::from(b"_57Z"),
                        }.into(),
                        Buffer {
                            size: ComputationalData::Byte(0x3d).into(),
                            initializer: &[0xf5, 0x83],
                        }.into(),
                    ],
                }
            );
        }

        #[test]
        fn test_mutex() {
            assert_errors!(N::parse, b"\x5b");
            assert_errors!(N::parse, b"\x5b\x01");
            assert_errors!(N::parse, b"\x5b\x01\x00");
            assert_parses!(N::parse, b"\x5b\x01\x00\x00", b"", N::Mutex {
                name: NameString::new(&[]),
                sync_level: 0,
            });
            assert_parses!(N::parse, b"\x5b\x01\\_BL8\x0f\x94\x7f", b"\x94\x7f", N::Mutex {
                name: NameString::new_root(&[NameSeg(*b"_BL8")]),
                sync_level: 0xf,
            });
        }

        #[test]
        fn test_operation_region() {
            assert_errors!(N::parse, b"\x5b");
            assert_errors!(N::parse, b"\x5b\x80");
            assert_errors!(N::parse, b"\x5b\x80\x00");
            assert_errors!(N::parse, b"\x5b\x80\x00\x00");
            assert_errors!(N::parse, b"\x5b\x80\x00\x00\x00");
            assert_parses!(N::parse, b"\x5b\x80\x00\x00\x00\x00", b"",
                N::OperationRegion {
                    name: NameString::new(&[]),
                    region_space: RegionSpace::SystemMemory,
                    offset: ComputationalData::Zero.into(),
                    length: ComputationalData::Zero.into(),
                }
            );
            assert_parses!(N::parse,
                //             rspace
                //        name |   offset  len rest
                //        |    |   |       |   |
                b"\x5b\x80^_J8M\xe8\x0a\x8b\xff\x05\x11",
                b"\x05\x11",
                N::OperationRegion {
                    name: NameString::new_parent(1, &[NameSeg(*b"_J8M")]),
                    region_space: RegionSpace::OEMDefined(0xe8),
                    offset: ComputationalData::Byte(0x8b).into(),
                    length: ComputationalData::Ones.into(),
                }
            );
        }

        #[test]
        fn test_power_resource() {
            assert_errors!(N::parse, b"\x5b");
            assert_errors!(N::parse, b"\x5b\x84");
            assert_errors!(N::parse, b"\x5b\x84\x00");
            assert_errors!(N::parse, b"\x5b\x84\x04");
            assert_errors!(N::parse, b"\x5b\x84\x01\x00");
            assert_errors!(N::parse, b"\x5b\x84\x04\x00");
            assert_errors!(N::parse, b"\x5b\x84\x02\x00\x00");
            assert_errors!(N::parse, b"\x5b\x84\x04\x00\x00");
            assert_errors!(N::parse, b"\x5b\x84\x03\x00\x00\x00");
            assert_errors!(N::parse, b"\x5b\x84\x04\x00\x00\x00");
            assert_parses!(N::parse, b"\x5b\x84\x04\x00\x00\x00\x00", b"",
                N::PowerResource {
                    name: NameString::new(&[]),
                    system_level: 0x00,
                    resource_order: 0x00,
                    body: vec![],
                }
            );
            assert_parses!(N::parse,
                //                  slevel
                //        pkg name  |   rorder  body[0]             body[1]                 rest
                //        |   |     |   |       |                   |                       |
                b"\x5b\x84\x16\\_487\x8f\xa2\x43\x8d\x63\x0a\x42_57Z\x11\x04\x0a\x3d\xf5\x83\x62\x01",
                b"\x62\x01",
                N::PowerResource {
                    name: NameString::new_root(&[NameSeg(*b"_487")]),
                    system_level: 0x8f,
                    resource_order: 0x43a2,
                    body: vec![
                        N::CreateBitField {
                            source_buffer: LocalObject::Local3.into(),
                            bit_index: ComputationalData::Byte(0x42).into(),
                            name: NameString::from(b"_57Z"),
                        }.into(),
                        Buffer {
                            size: ComputationalData::Byte(0x3d).into(),
                            initializer: &[0xf5, 0x83],
                        }.into(),
                    ],
                }
            );
        }

        #[test]
        fn test_processor() {
            assert_errors!(N::parse, b"\x5b");
            assert_errors!(N::parse, b"\x5b\x83");
            assert_errors!(N::parse, b"\x5b\x83\x00");
            assert_errors!(N::parse, b"\x5b\x83\x01\x00");
            assert_errors!(N::parse, b"\x5b\x83\x02\x00\x00");
            assert_errors!(N::parse, b"\x5b\x83\x03\x00\x00\x00");
            assert_errors!(N::parse, b"\x5b\x83\x04\x00\x00\x00\x00");
            assert_errors!(N::parse, b"\x5b\x83\x05\x00\x00\x00\x00\x00");
            assert_errors!(N::parse, b"\x5b\x83\x06\x00\x00\x00\x00\x00\x00");
            assert_parses!(N::parse, b"\x5b\x83\x07\x00\x00\x00\x00\x00\x00\x00", b"",
                N::Processor {
                    name: NameString::new(&[]),
                    id: 0x00,
                    register_block_addr: 0x0000_0000,
                    register_block_length: 0x00,
                    body: vec![],
                }
            );
            assert_parses!(N::parse,
                //                                     rlen
                //        pkg name id  raddr           |   body[0]                 body[1]     rest
                //        |   |    |   |               |   |                       |           |
                b"\x5b\x83\x17^_842\xf4\xa2\x83\x42\xed\xd2\x11\x04\x0a\x3d\xf5\x83\x5b\x02ABCDEFGH",
                b"EFGH",
                N::Processor {
                    name: NameString::new_parent(1, &[NameSeg(*b"_842")]),
                    id: 0xf4,
                    register_block_addr: 0xed42_83a2,
                    register_block_length: 0xd2,
                    body: vec![
                        Buffer {
                            size: ComputationalData::Byte(0x3d).into(),
                            initializer: &[0xf5, 0x83],
                        }.into(),
                        N::Event(NameString::new(&[NameSeg(*b"ABCD")])).into(),
                    ],
                }
            );
        }

        #[test]
        fn test_thermal_zone() {
            assert_errors!(N::parse, b"\x5b");
            assert_errors!(N::parse, b"\x5b\x85");
            assert_errors!(N::parse, b"\x5b\x85\x00");
            assert_errors!(N::parse, b"\x5b\x85\x01");
            assert_parses!(N::parse, b"\x5b\x85\x01\x00", b"",
                N::ThermalZone {
                    name: NameString::new(&[]),
                    body: vec![],
                }
            );
            assert_parses!(N::parse,
                //        pkg name  body[0]                 body[1]     rest
                //        |   |     |                       |           |
                b"\x5b\x85\x11\\_F37\x11\x04\x0a\x3d\xf5\x83\x5b\x02ABCDEFGH",
                b"EFGH",
                N::ThermalZone {
                    name: NameString::new_root(&[NameSeg(*b"_F37")]),
                    body: vec![
                        Buffer {
                            size: ComputationalData::Byte(0x3d).into(),
                            initializer: &[0xf5, 0x83],
                        }.into(),
                        N::Event(NameString::new(&[NameSeg(*b"ABCD")])).into(),
                    ],
                }
            );
        }
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
            let access_type = match AccessType::try_from(b & 0x0f) {
                Ok(a) => a,
                Err(_) => return err(i, ErrorKind::Tag),
            };
            let lock = b & 0x10 != 0;
            let update_rule = match UpdateRule::try_from((b & 0b0110_0000) >> 5) {
                Ok(u) => u,
                Err(_) => return err(i, ErrorKind::Tag),
            };
            Ok((i, FieldFlags { access_type, lock, update_rule }))
        });
    }

    #[cfg(test)]
    mod test_field_flags {
        use super::*;

        #[test]
        fn test_parse() {
            assert_errors!(FieldFlags::parse, &[]);
            assert_parses!(FieldFlags::parse,
                &[0],
                &[],
                FieldFlags {
                    access_type: AccessType::Any,
                    lock: false,
                    update_rule: UpdateRule::Preserve,
                },
            );
            assert_parses!(FieldFlags::parse,
                &[0b0010_0010],
                &[],
                FieldFlags {
                    access_type: AccessType::Word,
                    lock: false,
                    update_rule: UpdateRule::WriteAsOnes,
                },
            );
            assert_parses!(FieldFlags::parse,
                &[0b0101_0101, 0b1111_1111],
                &[0b1111_1111],
                FieldFlags {
                    access_type: AccessType::Buffer,
                    lock: true,
                    update_rule: UpdateRule::WriteAsZeros,
                },
            );
            // Most significant bit is ignored
            assert_parses!(FieldFlags::parse,
                &[0b1000_0000],
                &[],
                FieldFlags {
                    access_type: AccessType::Any,
                    lock: false,
                    update_rule: UpdateRule::Preserve,
                },
            );
            // Access type out of range
            assert_errors!(FieldFlags::parse, &[0b0000_0111]);
            assert_errors!(FieldFlags::parse, &[0b0000_1111]);
            // Update rule out of range
            assert_errors!(FieldFlags::parse, &[0b0110_0000]);
        }
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
    ///     merged into one enum variant. See [`access_field`] and
    ///     [`extended_access_field`].
    ///   * The ACPICA parser expects `BufferData` to be a `DefBuffer` op.
    ///   * The `ConnectField` op in split into two enum variants to avoid another level
    ///     of indirection.
    impl<'a> Parse<'a> for FieldElement<'a> {
        parser_fn! {
            parse<'a> -> Self = alt((
                struct_parser!(
                    Self::Named { name: NameSeg::parse, bit_length: parse_package_length }
                ),
                preceded(
                    tag_byte(0x00),
                    struct_parser!(Self::Reserved { bit_length: parse_package_length }),
                ),
                access_field,
                extended_access_field,
                preceded(
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
        access_field(i) -> FieldElement<'a> {
            let (i, _) = tag_byte(0x01)(i)?;
            let (i, first) = num::le_u8(i)?;
            let (i, second) = num::le_u8(i)?;

            let access_type = match AccessType::try_from(first & 0x0f) {
                Ok(a) => a,
                Err(_) => return err(i, ErrorKind::Tag),
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
        extended_access_field(i) -> FieldElement<'a> {
            let (i, _) = tag_byte(0x03)(i)?;
            let (i, type_byte) = num::le_u8(i)?;
            let (i, attrib_byte) = num::le_u8(i)?;
            let (i, length) = num::le_u8(i)?;

            let access_type = match AccessType::try_from(type_byte & 0x0f) {
                Ok(a) => a,
                Err(_) => return err(i, ErrorKind::Tag),
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
    }

    #[cfg(test)]
    mod test_field_element {
        use super::*;
        use super::FieldElement as F;
        use super::super::super::data::ComputationalData;

        #[test]
        fn test_misc_invalid() {
            assert_errors!(F::parse, b"");
            assert_errors!(F::parse, b"\xff");
            assert_errors!(F::parse, b"\x04");
        }

        #[test]
        fn test_named() {
            // Incomplete
            assert_errors!(F::parse, b"A");
            assert_errors!(F::parse, b"ABCD");

            assert_parses!(F::parse, b"ABCD\x00", b"", F::Named {
                name: b"ABCD".into(),
                bit_length: 0,
            });
            assert_parses!(F::parse, b"___1\x2d", b"", F::Named {
                name: b"___1".into(),
                bit_length: 0x2d,
            });
            assert_parses!(F::parse, b"X2__\xc7\x56\x34\x12zzz", b"zzz", F::Named {
                name: b"X2__".into(),
                bit_length: 0x0123_4567,
            });

            // Invalid name segment
            assert_errors!(F::parse, b"1BCD\x00");
            // Single segment only; path not allowed
            assert_errors!(F::parse, b"^ABCD\x00");
        }

        #[test]
        fn test_reserved() {
            assert_errors!(F::parse, b"\x00");
            assert_parses!(F::parse, b"\x00\x00",     b"",     F::Reserved { bit_length: 0x00 });
            assert_parses!(F::parse, b"\x00\x3f\xd7", b"\xd7", F::Reserved { bit_length: 0x3f });
            assert_parses!(F::parse,
                b"\x00\xc7\x56\x34\x12",
                b"",
                F::Reserved { bit_length: 0x0123_4567 },
            );
        }

        #[test]
        fn test_access_as() {
            assert_errors!(F::parse, b"\x01");
            assert_errors!(F::parse, b"\x01\x00");
            assert_parses!(F::parse, b"\x01\x00\x00", b"", F::AccessAs(
                AccessType::Any,
                AccessAttrib::None,
            ));
            assert_parses!(F::parse, b"\x01\xf5\xff", b"", F::AccessAs(
                AccessType::Buffer,
                AccessAttrib::RawProcessBytes(0xff),
            ));
            assert_parses!(F::parse, b"\x01\x03\x0a\xd5", b"\xd5", F::AccessAs(
                AccessType::DWord,
                AccessAttrib::Block,
            ));
            assert_parses!(F::parse, b"\x01\x81\x0a", b"", F::AccessAs(
                AccessType::Byte,
                AccessAttrib::RawBytes(0x0a),
            ));
            assert_parses!(F::parse, b"\x01\x42\xe9", b"", F::AccessAs(
                AccessType::Word,
                AccessAttrib::Bytes(0xe9),
            ));

            // Invalid access type
            assert_errors!(F::parse, b"\x01\x06\x00");
            assert_errors!(F::parse, b"\x01\x0b\x00");
            assert_errors!(F::parse, b"\x01\x0f\x00");

            // Invalid attribute
            assert_errors!(F::parse, b"\x01\x00\x01");
            assert_errors!(F::parse, b"\x01\x00\x03");
            assert_errors!(F::parse, b"\x01\x00\x05");
            assert_errors!(F::parse, b"\x01\x00\x07");
            assert_errors!(F::parse, b"\x01\x00\x09");
            assert_errors!(F::parse, b"\x01\x00\x0b");
            assert_errors!(F::parse, b"\x01\x00\x0e");
            assert_errors!(F::parse, b"\x01\x00\x0f");
        }

        #[test]
        fn test_access_as_extended() {
            assert_errors!(F::parse, b"\x03");
            assert_errors!(F::parse, b"\x03\x00");
            assert_errors!(F::parse, b"\x03\x00\x00");
            assert_errors!(F::parse, b"\x03\x00\x0b");
            assert_parses!(F::parse, b"\x03\x00\x0b\x00", b"", F::AccessAs(
                AccessType::Any,
                AccessAttrib::Bytes(0),
            ));
            assert_parses!(F::parse, b"\x03\x05\x0f\xff", b"", F::AccessAs(
                AccessType::Buffer,
                AccessAttrib::RawProcessBytes(0xff),
            ));
            assert_parses!(F::parse, b"\x03\x02\x0e\xc8\x3f", b"\x3f", F::AccessAs(
                AccessType::Word,
                AccessAttrib::RawBytes(0xc8),
            ));

            // Invalid access type
            assert_errors!(F::parse, b"\x03\x06\x0b\x00");
            assert_errors!(F::parse, b"\x03\x0d\x0b\x00");
            assert_errors!(F::parse, b"\x03\x0f\x0b\x00");

            // Invalid attributes
            assert_errors!(F::parse, b"\x03\x00\x00\x00");
            assert_errors!(F::parse, b"\x03\x00\x01\x00");
            assert_errors!(F::parse, b"\x03\x00\x0c\x00");
            assert_errors!(F::parse, b"\x03\x00\x0d\x00");
        }

        #[test]
        fn test_connect_named() {
            let abcd = NameSeg(*b"ABCD");
            let x123 = NameSeg(*b"X123");

            assert_errors!(F::parse, b"\x02");
            assert_errors!(F::parse, b"\x02\xff");
            assert_parses!(F::parse,
                b"\x02\x00",
                b"",
                F::ConnectNamed(NameString::new(&[])),
            );

            assert_errors!(F::parse, b"\x02A");
            assert_errors!(F::parse, b"\x02AB");
            assert_errors!(F::parse, b"\x02ABC");
            assert_parses!(F::parse,
                b"\x02ABCD",
                b"",
                F::ConnectNamed(abcd.into()),
            );
            assert_parses!(F::parse,
                b"\x02\\X123A567",
                b"A567",
                F::ConnectNamed(NameString::new_root(&[x123])),
            );
            assert_parses!(F::parse,
                b"\x02^^^^\x2eABCDX123",
                b"",
                F::ConnectNamed(NameString::new_parent(4, &[abcd, x123])),
            );

            // Missing a name segment
            assert_errors!(F::parse, b"\x02\x2eABCD");
        }

        #[test]
        fn test_connect_buffer() {
            assert_errors!(F::parse, b"\x02\x11");
            assert_errors!(F::parse, b"\x02\x11\x00");
            assert_errors!(F::parse, b"\x02\x11\x01");
            assert_parses!(F::parse, b"\x02\x11\x01\x00", b"", F::ConnectBuffer(Buffer {
                size: ComputationalData::Zero.into(),
                initializer: &[],
            }));
            assert_parses!(F::parse, b"\x02\x11\x03\x01\x4d\xa9", b"", F::ConnectBuffer(Buffer {
                size: ComputationalData::One.into(),
                initializer: &[0x4d, 0xa9],
            }));
            assert_errors!(F::parse, b"\x02\x11\x03\x01\x4d");

            // Could be interpreted as ASCII (like a NameSeg), but shouldn't be
            assert_parses!(F::parse, b"\x02\x11\tbcdefgabc", b"", F::ConnectBuffer(Buffer {
                size: LocalObject::Local2.into(),
                initializer: b"cdefgabc",
            }));
        }
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

    #[cfg(test)]
    mod test_method_flags {
        use super::*;

        #[test]
        fn test_parse() {
            assert_errors!(MethodFlags::parse, &[]);
            assert_parses!(MethodFlags::parse,
                &[0b0000_0000],
                &[],
                MethodFlags { arg_count: 0, serialized: false, sync_level: 0x0 },
            );
            assert_parses!(MethodFlags::parse,
                &[0b0001_1001],
                &[],
                MethodFlags { arg_count: 1, serialized: true, sync_level: 0x1 },
            );
            assert_parses!(MethodFlags::parse,
                &[0b1111_0111, 0b1010_0101],
                &[0b1010_0101],
                MethodFlags { arg_count: 7, serialized: false, sync_level: 0xf },
            );
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
        fatal -> StatementOpcode<'a> = preceded(
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
        if_op(i) -> StatementOpcode<'a> {
            let (i, _) = tag_byte(0xa0)(i)?;
            let (i, (predicate, if_true)) = in_package(tuple((
               TermArg::parse,
               multi::many0(TermObject::parse),
            )))(i)?;
            let (i, if_false) = opt(in_package(preceded(
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
    }

    parser_fn! {
        /// ```text
        /// DefLoad         := LoadOp NameString DDBHandleObject
        /// LoadOp          := ExtOpPrefix 0x20
        /// DDBHandleObject := SuperName
        /// ```
        load -> StatementOpcode<'a> = preceded(
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
        notify -> StatementOpcode<'a> = preceded(
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
        release -> StatementOpcode<'a> = preceded(
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
        reset -> StatementOpcode<'a> = preceded(
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
        return_op -> StatementOpcode<'a> = preceded(
            tag_byte(0xa4),
            map(TermArg::parse, StatementOpcode::Return)
        )
    }

    parser_fn! {
        /// ```text
        /// DefSignal       := SignalOp EventObject
        /// SignalOp        := ExtOpPrefix 0x24
        /// ```
        signal -> StatementOpcode<'a> = preceded(
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
        sleep -> StatementOpcode<'a> = preceded(
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
        stall -> StatementOpcode<'a> = preceded(
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
        while_op -> StatementOpcode<'a> = preceded(
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
        ref_of -> ReferenceExpressionOpcode<'a> = preceded(
            tag_byte(0x71),
            map(SuperName::parse, ReferenceExpressionOpcode::RefOf),
        )
    }

    parser_fn! {
        /// ```text
        /// DefDerefOf          := DerefOfOp ObjReference
        /// DerefOfOp           := 0x83
        /// ObjReference        := TermArg => ??ObjectReference | String
        /// ```
        deref -> ReferenceExpressionOpcode<'a> = preceded(
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
        index -> ReferenceExpressionOpcode<'a> = preceded(
            tag_byte(0x83),
            struct_parser! {
                ReferenceExpressionOpcode::DefIndex {
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
        // TODO: Fix ambiguous grammar
        invoke -> ReferenceExpressionOpcode<'a> = |i| err(i, ErrorKind::Tag)
        // invoke -> ReferenceExpressionOpcode<'a> = struct_parser! {
        //     ReferenceExpressionOpcode::Invoke {
        //         source: NameString::parse,
        //         args: multi::many0(TermArg::parse),
        //     }
        // }
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
        [$( #[$meta:meta] )* $parser:ident $op:ident $tag:literal] => {
            parser_fn! {
                $(#[$meta])*
                $parser -> ExpressionOpcode<'a> = preceded(
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
        add Add 0x72
    }

    binary_op_parser! {
        /// ```text
        /// DefAnd              := AndOp Operand Operand Target
        /// AndOp               := 0x7B
        /// ```
        bitwise_and BitwiseAnd 0x7b
    }

    binary_op_parser! {
        /// ```text
        /// DefOr               := OrOp Operand Operand Target
        /// OrOp                := 0x7D
        /// ```
        bitwise_or BitwiseOr 0x7d
    }

    binary_op_parser! {
        /// ```text
        /// DefXOr              := XorOp Operand Operand Target
        /// XorOp               := 0x7F
        /// ```
        bitwise_xor BitwiseXor 0x7f
    }

    binary_op_parser! {
        /// ```text
        /// DefConcat           := ConcatOp Data Data Target
        /// ConcatOp            := 0x73
        /// ```
        /// Data                := TermArg => ComputationalData
        concat Concat 0x73
    }

    binary_op_parser! {
        /// ```text
        /// DefConcatRes        := ConcatResOp BufData BufData Target
        /// ConcatResOp         := 0x84
        /// ```
        /// BufData             := TermArg => Buffer
        concat_res ConcatRes 0x84
    }

    binary_op_parser! {
        /// ```text
        /// DefMod              := ModOp Dividend Divisor Target
        /// ModOp               := 0x85
        /// ```
        mod_op Mod 0x85
    }

    binary_op_parser! {
        /// ```text
        /// DefMultiply         := MultiplyOp Operand Operand Target
        /// MultiplyOp          := 0x77
        /// ```
        multiply Multiply 0x77
    }

    binary_op_parser! {
        /// ```text
        /// DefNAnd             := NandOp Operand Operand Target
        /// NandOp              := 0x7C
        /// ```
        nand Nand 0x7c
    }

    binary_op_parser! {
        /// ```text
        /// DefNOr              := NorOp Operand Operand Target
        /// NorOp               := 0x7E
        /// ```
        nor Nor 0x7e
    }

    binary_op_parser! {
        /// ```text
        /// DefShiftLeft        := ShiftLeftOp Operand ShiftCount Target
        /// ShiftLeftOp         := 0x79
        /// ShiftCount          := TermArg => Integer
        /// ```
        shift_left ShiftLeft 0x79
    }

    binary_op_parser! {
        /// ```text
        /// DefShiftRight       := ShiftRightOp Operand ShiftCount Target
        /// ShiftRightOp        := 0x7A
        /// ```
        shift_right ShiftRight 0x7a
    }

    binary_op_parser! {
        /// ```text
        /// DefSubtract         := SubtractOp Operand Operand Target
        /// SubtractOp          := 0x74
        /// ```
        subtract Subtract 0x74
    }

    macro_rules! unary_op_parser {
        [$( #[$meta:meta] )* $parser:ident $op:ident $tag:literal] => {
            unary_op_parser! { $(#[$meta])* $parser $op tag_byte($tag) }
        };

        [$( #[$meta:meta] )* $parser:ident $op:ident $tag:expr] => {
            parser_fn! {
                $(#[$meta])*
                $parser -> ExpressionOpcode<'a> = preceded(
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
        bitwise_not BitwiseNot 0x80
    }

    unary_op_parser! {
        /// ```text
        /// DefFindSetLeftBit   := FindSetLeftBitOp Operand Target
        /// FindSetLeftBitOp    := 0x81
        /// ```
        find_set_left_bit FindSetLeftBit 0x81
    }

    unary_op_parser! {
        /// ```text
        /// DefFindSetRightBit  := FindSetRightBitOp Operand Target
        /// FindSetRightBitOp   := 0x82
        /// ```
        find_set_right_bit FindSetRightBit 0x82
    }

    unary_op_parser! {
        /// ```text
        /// DefFromBCD          := FromBCDOp BCDValue Target
        /// FromBCDOp           := ExtOpPrefix 0x28
        /// ```
        from_bcd FromBCD ext_op(tag_byte(0x28))
    }

    unary_op_parser! {
        /// ```text
        /// DefToBCD            := ToBCDOp Operand Target
        /// ToBCDOp             := ExtOpPrefix 0x29
        /// BCDValue            := TermArg => Integer
        /// ```
        to_bcd ToBCD ext_op(tag_byte(0x29))
    }

    unary_op_parser! {
        /// ```text
        /// DefToBuffer         := ToBufferOp Operand Target
        /// ToBufferOp          := 0x96
        /// ```
        to_buffer ToBuffer 0x96
    }

    unary_op_parser! {
        /// ```text
        /// DefToDecimalString  := ToDecimalStringOp Operand Target
        /// ToDecimalStringOp   := 0x97
        /// ```
        to_decimal_string ToDecimalString 0x97
    }

    unary_op_parser! {
        /// ```text
        /// DefToHexString      := ToHexStringOp Operand Target
        /// ToHexStringOp       := 0x98
        /// ```
        to_hex_string ToHexString 0x98
    }

    unary_op_parser! {
        /// ```text
        /// DefToInteger        := ToIntegerOp Operand Target
        /// ToIntegerOp         := 0x99
        /// ```
        to_integer ToInteger 0x99
    }

    macro_rules! logical_op_parser {
        [$( #[$meta:meta] )* $parser:ident $op:ident $tag:literal] => {
            parser_fn! {
                $(#[$meta])*
                $parser -> ExpressionOpcode<'a> = preceded(
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
        equal Equal 0x93
    }

    logical_op_parser! {
        /// ```text
        /// DefLGreater         := LgreaterOp Operand Operand
        /// LgreaterOp          := 0x94
        /// ```text
        greater Greater 0x94
    }

    logical_op_parser! {
        /// ```text
        /// DefLLess            := LlessOp Operand Operand
        /// LlessOp             := 0x95
        /// ```text
        less Less 0x95
    }

    logical_op_parser! {
        /// ```text
        /// DefLAnd             := LandOp Operand Operand
        /// LandOp              := 0x90
        /// ```text
        logical_and LogicalAnd 0x90
    }

    logical_op_parser! {
        /// ```text
        /// DefLOr              := LorOp Operand Operand
        /// LorOp               := 0x91
        /// ```text
        logical_or LogicalOr 0x91
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
        inverted_logical_ops -> ExpressionOpcode<'a> = preceded(
            tag_byte(0x92),
            alt((
                preceded(tag_byte(0x95), struct_parser!(ExpressionOpcode::GreaterEqual(TermArg::parse, TermArg::parse))),
                preceded(tag_byte(0x94), struct_parser!(ExpressionOpcode::LessEqual(TermArg::parse, TermArg::parse))),
                preceded(tag_byte(0x93), struct_parser!(ExpressionOpcode::NotEqual(TermArg::parse, TermArg::parse))),
                map(TermArg::parse, ExpressionOpcode::LogicalNot),
            ))
        )
    }

    parser_fn! {
        /// ```text
        /// DefDecrement        := DecrementOp SuperName
        /// DecrementOp         := 0x76
        /// ```
        decrement -> ExpressionOpcode<'a> = preceded(
            tag_byte(0x76),
            map(SuperName::parse, ExpressionOpcode::Decrement),
        )
    }

    parser_fn! {
        /// ```text
        /// DefIncrement        := IncrementOp SuperName
        /// IncrementOp         := 0x75
        /// ```
        increment -> ExpressionOpcode<'a> = preceded(
            tag_byte(0x75),
            map(SuperName::parse, ExpressionOpcode::Increment),
        )
    }

    parser_fn! {
        /// ```text
        /// DefSizeOf           := SizeOfOp SuperName
        /// SizeOfOp            := 0x87
        /// ```
        size_of -> ExpressionOpcode<'a> = preceded(
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
        object_type -> ExpressionOpcode<'a> = preceded(
            tag_byte(0x8e),
            map(
                verify(
                    SuperName::parse,
                    // Disallow method invocations
                    |n| match n {
                        SuperName::Reference(r) =>
                            !matches!(
                                **r,
                                ReferenceExpressionOpcode::Invoke { source: _, args: _ }
                            ),
                        _ => true,
                    }
                ),
                ExpressionOpcode::ObjectType
            )
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
        acquire -> ExpressionOpcode<'a> = preceded(
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
        wait -> ExpressionOpcode<'a> = preceded(
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
        cond_ref_of -> ExpressionOpcode<'a> = preceded(
            ext_op(tag_byte(0x12)),
            struct_parser! {
                ExpressionOpcode::CondRefOf(SuperName::parse, parse_target)
            }
        )
    }

    parser_fn! {
        /// ```text
        /// DefCopyObject       := CopyObjectOp TermArg SimpleName
        /// CopyObjectOp        := 0x9D
        /// ```
        copy_object -> ExpressionOpcode<'a> = preceded(
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
        divide -> ExpressionOpcode<'a> = preceded(
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
        load_table -> ExpressionOpcode<'a> = preceded(
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
        match_op -> ExpressionOpcode<'a> = preceded(
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
        mid -> ExpressionOpcode<'a> = preceded(
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
        store -> ExpressionOpcode<'a> = preceded(
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
        to_string -> ExpressionOpcode<'a> = preceded(
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
    use super::*;
    use super::super::misc::*;
    use super::util::*;

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

    #[cfg(test)]
    mod test_arg_obj {
        use super::*;
        use super::ArgObject::*;

        #[test]
        fn test_parse() {
            assert_errors!(ArgObject::parse, &[]);

            assert_errors!(ArgObject::parse, &[0x67]);
            assert_parses!(ArgObject::parse, &[0x68], &[], Arg0);
            assert_parses!(ArgObject::parse, &[0x6c], &[], Arg4);
            assert_parses!(ArgObject::parse, &[0x6e], &[], Arg6);
            assert_errors!(ArgObject::parse, &[0x6f]);

            let data = &[0x6c, 0x6e, 0x48, 0x68];
            assert_parses!(ArgObject::parse, &data[0..], &data[1..], Arg4);
            assert_parses!(ArgObject::parse, &data[1..], &data[2..], Arg6);
            assert_errors!(ArgObject::parse, &data[2..]);
            assert_parses!(ArgObject::parse, &data[3..], &data[4..], Arg0);
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
