use core::cmp::min;
use core::fmt;
use nom::error::{ContextError, ErrorKind, ParseError, VerboseError, VerboseErrorKind};
use nom::{AsBytes, IResult};


/// Trait alias combining the nom error traits [`ParseError`](nom::error::ParseError) and
/// [`ContextError`](nom::error::ContextError).
#[allow(clippy::module_name_repetitions)]
pub trait GeneralParseError<I>
where
    Self: ParseError<I> + ContextError<I>,
{
}

impl<T, I> GeneralParseError<I> for T where T: ParseError<I> + ContextError<I> {}


/// Shortcut for constructing parser errors
#[allow(clippy::missing_errors_doc)]
pub fn err<I, O, E: ParseError<I>>(i: I, kind: ErrorKind) -> IResult<I, O, E> {
    Err(nom::Err::Error(E::from_error_kind(i, kind)))
}


/// Helper struct that prints human-readable position information when formatted with
/// `Display`.
pub struct Position<'a, I: AsBytes = &'a [u8]> {
    /// The state of the parser at the time we are interested in
    pub state: &'a I,
    /// The complete input we are trying to parse
    pub full_input: &'a [u8],
}

impl<I: AsBytes> fmt::Display for Position<'_, I> {
    fn fmt(&self, out: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        if self.full_input.is_empty() {
            return writeln!(out, "in empty input");
        }

        // NOTE: nom's Offset implementation can panic. Avoid it.
        let full_input_pos = self.full_input.as_ptr() as usize;
        let state_pos = self.state.as_bytes().as_ptr() as usize;
        let offset = match state_pos.checked_sub(full_input_pos) {
            Some(o) => o,
            None => return writeln!(out, "at unknown/invalid offset"),
        };

        let context_start = offset.saturating_sub(10);
        let context_end = min(offset + 10, self.full_input.len());
        let context_slice = &self.full_input[context_start..context_end];

        write!(out, "at offset {offset} ({offset:#x}):\n ")?;
        for byte in context_slice {
            let ascii_char = match byte {
                b if b.is_ascii_graphic() => *b as char,
                b' ' => ' ',
                _ => '.',
            };
            write!(out, " {ascii_char:2}")?;
        }
        write!(out, "\n ")?;
        for byte in context_slice {
            write!(out, " {byte:02x}")?;
        }
        write!(
            out,
            "\n  {caret:>caret_offset$}\n",
            caret = '^',
            caret_offset = (offset - context_start) * 3 + 1,
        )
    }
}


/// A parser error with position info useful for printing human-readable messages.
#[derive(Debug, Clone)]
#[allow(clippy::module_name_repetitions)]
pub struct ErrorWithPosition<'a, I = &'a [u8]>
where
    I: From<&'a [u8]> + AsBytes,
{
    /// The error returned by the parser
    pub error: VerboseError<I>,
    /// The complete input we were trying to parse
    pub full_input: &'a [u8],
}

impl<'a, I> ErrorWithPosition<'a, I>
where
    I: From<&'a [u8]> + AsBytes,
{
    /// Associate a parse error with its full input, to reference the affected position
    /// in error messages.
    pub fn new(wrapped_error: nom::Err<VerboseError<I>>, full_input: &'a [u8]) -> Self {
        match wrapped_error {
            nom::Err::Error(error) | nom::Err::Failure(error) => {
                ErrorWithPosition { error, full_input }
            }
            nom::Err::Incomplete(_) => {
                let error = VerboseError::from_error_kind(
                    I::from(full_input),
                    ErrorKind::Complete,
                );
                ErrorWithPosition { error, full_input }
            }
        }
    }
}

impl<'a, I> fmt::Display for ErrorWithPosition<'a, I>
where
    I: From<&'a [u8]> + AsBytes,
{
    fn fmt(&self, out: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        if self.error.errors.is_empty() {
            return writeln!(out, "Unknown parsing error");
        }

        for (state, kind) in &self.error.errors {
            match kind {
                VerboseErrorKind::Context(context) => {
                    write!(out, "In {context}")?;
                }
                VerboseErrorKind::Char(c) => {
                    write!(out, "Expected '{c}'")?;
                }
                VerboseErrorKind::Nom(e) => {
                    write!(out, "Failed {}", e.description())?;
                }
            }
            write!(out, " {}", Position { state, full_input: self.full_input })?;
        }

        Ok(())
    }
}
