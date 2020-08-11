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
use nom::combinator::{all_consuming, flat_map, map, rest, value, verify};
use nom::error::{ErrorKind, ParseError};
use nom::number::complete as num;
use nom::multi;
use nom::sequence::preceded;


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
                    ($imp)(i)
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
            assert_parses!(NS::parse, b"\0",   b"",  NS::new(vec![]));
            assert_parses!(NS::parse, b"\0K",  b"K", NS::new(vec![]));
            assert_parses!(NS::parse, b"^^\0", b"",  NS::new_parent(2, vec![]));
            assert_parses!(NS::parse, b"\\\0", b"",  NS::new_root(vec![]));
        }

        #[test]
        fn test_parse_single() {
            let ns = vec![NameSeg(*b"A123")];
            assert_parses!(NS::parse, b"A123",    b"",  NS::new(ns.clone()));
            assert_parses!(NS::parse, b"A1234",   b"4", NS::new(ns.clone()));
            assert_parses!(NS::parse, b"^^^A123", b"",  NS::new_parent(3, ns.clone()));
            assert_parses!(NS::parse, b"\\A123",  b"",  NS::new_root(ns.clone()));
        }

        #[test]
        fn test_parse_dual() {
            let ns = vec![NameSeg(*b"A___"), NameSeg(*b"B___")];
            assert_parses!(NS::parse, b"\x2eA___B___",     b"",     NS::new(ns.clone()));
            assert_parses!(NS::parse, b"\x2eA___B___C",    b"C",    NS::new(ns.clone()));
            assert_parses!(NS::parse, b"\x2eA___B___C___", b"C___", NS::new(ns.clone()));
            assert_parses!(NS::parse, b"\\\x2eA___B___",   b"",     NS::new_root(ns.clone()));
            assert_parses!(NS::parse, b"^^^\x2eA___B___",  b"",     NS::new_parent(3, ns.clone()));

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
            assert_parses!(NS::parse, b"\x2f\x00",     b"",     NS::new(vec![]));
            assert_parses!(NS::parse, b"\x2f\x00A___", b"A___", NS::new(vec![]));
            assert_parses!(NS::parse, b"^^\x2f\x00",   b"",     NS::new_parent(2, vec![]));
            assert_parses!(NS::parse, b"\\\x2f\x00",   b"",     NS::new_root(vec![]));

            // Count = 1
            assert_errors!(NS::parse, b"\x2f\x01");
            assert_errors!(NS::parse, b"\x2f\x01A");
            assert_errors!(NS::parse, b"\x2f\x01A_");
            assert_errors!(NS::parse, b"\x2f\x01A__");
            assert_parses!(NS::parse, b"\x2f\x01A___",     b"",     NS::new(vec![a]));
            assert_parses!(NS::parse, b"\x2f\x01A___",     b"",     NS::new(vec![a]));
            assert_parses!(NS::parse, b"\x2f\x01A___B",    b"B",    NS::new(vec![a]));
            assert_parses!(NS::parse, b"\x2f\x01A___B___", b"B___", NS::new(vec![a]));
            assert_parses!(NS::parse, b"\x2f\x01A___",     b"",     NS::new(vec![a]));

            // Count = 4
            assert_errors!(NS::parse, b"\x2f\x04");
            assert_errors!(NS::parse, b"\x2f\x04A___B___C___D__");
            assert_parses!(NS::parse, b"\x2f\x04A___B___C___D___",     b"",     NS::new(vec![a, b, c, d]));
            assert_parses!(NS::parse, b"\x2f\x04A___B___C___D___E",    b"E",    NS::new(vec![a, b, c, d]));
            assert_parses!(NS::parse, b"\x2f\x04A___B___C___D___E___", b"E___", NS::new(vec![a, b, c, d]));
            assert_parses!(NS::parse, b"\\\x2f\x04A___B___C___D___",   b"",     NS::new_root(vec![a, b, c, d]));
            assert_parses!(NS::parse, b"^\x2f\x04A___B___C___D___",    b"",     NS::new_parent(1, vec![a, b, c, d]));
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
        #[ignore]
        fn test_buffer() {
            // TODO: Implement
            unimplemented!();
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
            parse<'a> -> Self = in_package(struct_parser! {
                Buffer {
                    size: TermArg::parse,
                    initializer: rest,
                }
            })
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
            parse<'a> -> Self = in_package(struct_parser! {
                Package {
                    count: num::le_u8,
                    initializers: multi::many0(PackageElement::parse),
                }
            })
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
            parse<'a> -> Self = in_package(struct_parser! {
                VarPackage {
                    count: TermArg::parse,
                    initializers: multi::many0(PackageElement::parse),
                }
            })
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
    impl<'a> Parse<'a> for DataRefObject<'a> {
        parser_fn!(parse('a i) -> Self {
            // TODO: Implement
            err(i, ErrorKind::Verify)
        });
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
    use super::*;
    use super::util::*;
    use super::package::in_package;
    use super::super::term::*;
    use super::super::data::DataRefObject;
    use super::super::name::NameString;


    /// Grammar:
    ///
    /// ```text
    /// TermObj  := Object | Type1Opcode | Type2Opcode
    /// TermList := Nothing | <TermObj TermList>
    /// Object   := NameSpaceModifierObj | NamedObj
    /// ```
    impl<'a> Parse<'a> for TermObject<'a> {
        parser_fn!(parse('a i) -> Self {
            // TODO: Implement
            err(i, ErrorKind::Verify)
        });
    }


    /// Grammar:
    ///
    /// ```text
    /// TermArg     := Type2Opcode | DataObject | ArgObj | LocalObject
    /// TermArgList := Nothing | <TermArg TermArgList>
    /// ```
    impl<'a> Parse<'a> for TermArg<'a> {
        parser_fn!(parse('a i) -> Self {
            // TODO: Implement
            err(i, ErrorKind::Verify)
        });
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
    impl<'a> Parse<'a> for NamedObject<'a> {
        parser_fn!(parse('a i) -> Self {
            // TODO: Implement
            err(i, ErrorKind::Verify)
        });
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
            let access_type = AccessType::try_from(b & 0b0000_1111)
                .map_err(|_| nom::Err::Error(E::from_error_kind(i, ErrorKind::Tag)))?;
            let lock = b & 0b0001_0000 != 0;
            let update_rule = UpdateRule::try_from((b & 0b0110_0000) >> 5)
                .map_err(|_| nom::Err::Error(E::from_error_kind(i, ErrorKind::Tag)))?;
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
    /// the ASL grammar. See the [`AccessAttrib`] struct for the AML grammar.
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
    /// FieldList    := Nothing | <FieldElement FieldList>
    /// FieldElement := NamedField | ReservedField | AccessField | ExtendedAccessField |
    ///                 ConnectField
    /// ```
    impl<'a> Parse<'a> for FieldElement<'a> {
        parser_fn!(parse('a i) -> Self {
            // TODO: Implement
            err(i, ErrorKind::Verify)
        });
    }


    /// The AML spec is kind of a mess here, and the same information can apparently be
    /// encoded in multiple ways. This enum reflects the ASL grammar. I don't know whether
    /// the compiler prefers one representation over the other, but we'll assume there is
    /// no real difference.
    ///
    /// Grammar:
    ///
    /// ```text
    /// AccessType    := ByteData
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
    ///
    /// ExtendedAccessAttrib := ByteData
    ///     // 0x0B AttribBytes
    ///     // 0x0E AttribRawBytes
    ///     // 0x0F AttribRawProcess
    /// ```
    impl<'a> Parse<'a> for AccessAttrib {
        parser_fn!(parse('a i) -> Self {
            // TODO: Implement
            err(i, ErrorKind::Verify)
        });
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
        parser_fn!(parse('a i) -> Self {
            // TODO: Implement
            err(i, ErrorKind::Verify)
        });
    }


    /// Grammar:
    ///
    /// ```text
    /// Type6Opcode := DefRefOf | DefDerefOf | DefIndex | ??UserTermObj
    /// ```
    impl<'a> Parse<'a> for ReferenceExpressionOpcode<'a> {
        parser_fn!(parse('a i) -> Self {
            // TODO: Implement
            err(i, ErrorKind::Verify)
        });
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
        parser_fn!(parse('a i) -> Self {
            // TODO: Implement
            err(i, ErrorKind::Verify)
        });
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


    // TODO: Does this actually need a parser?
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
