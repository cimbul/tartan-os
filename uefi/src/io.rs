//! [`SimpleTextOutput`]-based [`Write`] implementation to support formatting macros.

use core::fmt::Write;
use super::{Result, Status};
use super::proto::SimpleTextOutput;

#[macro_export]
/// Write formatted data to an [`OutputStream`] and return the last result, rather than a
/// dumb [`core::fmt::Error`].
macro_rules! writeln_result {
    [$out:ident, $($args:expr),* $(,)?] => {
        match writeln!($out, $($args),*) {
            _ => $out.last_result
        }
    }
}

pub struct OutputStream<'a> {
    out: &'a SimpleTextOutput,
    pub last_result: Result,
}

impl<'a> OutputStream<'a> {
    pub fn new(out: &'a SimpleTextOutput) -> Self {
        OutputStream { out, last_result: Ok(Status::SUCCESS) }
    }
}

impl Write for OutputStream<'_> {
    fn write_str(&mut self, s: &str) -> core::fmt::Result {
        // TODO: Horribly inefficient, but simplest from a stack allocation perspective
        for c in s.chars() {
            if let e@Err(_) = self.write_char(c) {
                return e;
            }
        };
        Ok(())
    }

    fn write_char(&mut self, c: char) -> core::fmt::Result {
        // Automatically translate LF to CRLF. Internally-used panic strings can contain
        // line breaks (e.g., assert_eq!), and this ensures they are formatted correctly.
        if c == '\n' {
            self.write_char('\r')?;
        }

        // Two for UTF-16 code point (possibly a surrogate pair). One for null terminator.
        let mut buffer = [0_u16; 3];
        c.encode_utf16(&mut buffer);
        let out = &self.out;
        unsafe {
            self.last_result = (out.output_string)(out, buffer.as_ptr()).into_result();
        }
        match self.last_result {
            Ok(_) => Ok(()),
            Err(_) => Err(core::fmt::Error),
        }
    }
}
