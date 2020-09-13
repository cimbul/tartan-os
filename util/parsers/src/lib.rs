//! Utilities for nom parsers

#![no_std]
#![warn(missing_docs)]
#![warn(clippy::pedantic)]
#![allow(clippy::must_use_candidate)]

use nom::error::ParseError;
use nom::sequence::preceded;
use nom::IResult;


/// Helpers for reporting parsing errors
pub mod error;


/// Parse struct fields in order using an initializer-like syntax
///
/// # Example
///
/// ```
/// # use nom::IResult;
/// # use nom::character::complete::alphanumeric1;
/// # use nom::number::complete::le_u16;
/// # use tartan_parsers::struct_parser;
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


/// Transforms a parser error (recoverable) to a failure (non-recoverable). This
/// avoids pointless backtracking when we know that no alternatives will succeed.
///
/// Unlike [`nom::combinator::cut`], this doesn't duplicate the input for no apparent
/// reason.
pub fn cut<P, I, O, E>(parser: P) -> impl Fn(I) -> IResult<I, O, E>
where
    P: Fn(I) -> IResult<I, O, E>,
    E: ParseError<I>,
{
    move |i| match parser(i) {
        Err(nom::Err::Error(e)) => Err(nom::Err::Failure(e)),
        other => other,
    }
}


/// Combinator for productions that use a deterministic opcode
///
/// If a production is preceded by an unambiguous opcode, then we can avoid
/// backtracking when parsing the rest. This enables better error messages in addition
/// to faster parsing.
pub fn opcode<I, O1, O2, E, P, Q>(
    description: &'static str,
    opcode_parser: P,
    body_parser: Q,
) -> impl Fn(I) -> IResult<I, O2, E>
where
    I: Clone,
    P: Fn(I) -> IResult<I, O1, E>,
    Q: Fn(I) -> IResult<I, O2, E>,
    E: ParseError<I>,
{
    move |i| match preceded(&opcode_parser, cut(&body_parser))(i.clone()) {
        Err(nom::Err::Failure(e)) => {
            // Only add context to *failures*, on the assumption that these will only
            // come from the opcode body, and we don't want to add context when the
            // opcode itself wasn't recognized.
            Err(nom::Err::Failure(E::add_context(i, description, e)))
        }
        other => other,
    }
}
