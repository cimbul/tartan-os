macro_rules! assert_parses {
    ($parser:expr, $input:expr, $rest:expr, $output:expr $(,)?) => {
        let parser = $parser;
        let input = $input;
        let expected_rest = $rest;
        let expected_output = $output;
        let result: nom::IResult<_, _, ()> = parser(
            crate::acpi::aml::parse::state::ParserState::new(input));
        match result {
            Err(_) => panic!(
                "\nInput could not be parsed\n   input: {:?}\n  wanted: {:?}\n  parser: {}\n",
                input,
                expected_output,
                stringify!($parser),
            ),
            Ok((output_state, actual_output)) => {
                assert!(
                    actual_output == expected_output,
                    "\nDid not get expected output from parser\n  wanted: {:?}\n     got: {:?}\n   input: {:?}\n    rest: {:?}\n  parser: {}\n",
                    expected_output,
                    actual_output,
                    input,
                    output_state.data,
                    stringify!($parser),
                );
                assert!(
                    output_state.data == expected_rest,
                    "\nParser did not consume expected data\n  wanted: {:?}\n     got: {:?}\n   input: {:?}\n  output: {:?}\n  parser: {}\n",
                    expected_rest,
                    output_state.data,
                    input,
                    actual_output,
                    stringify!($parser),
                );
            }
        }
    };
}

macro_rules! assert_errors {
    ($parser:expr, $input:expr) => {
        let parser = $parser;
        let input = $input;
        let result: nom::IResult<_, _, ()> = parser(
            crate::acpi::aml::parse::state::ParserState::new(input));
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


mod util {
    use crate::acpi::aml::parse::util::*;
    use crate::acpi::aml::parse::state::ParserState;
    use nom::bytes::complete as bytes;

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
        let ext_xyz = ext_op(ParserState::lift(bytes::is_a("XYZ")));
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


mod name {
    use crate::acpi::aml::parse::Parse;
    use crate::acpi::aml::name::{NameSeg, NameString, PathAnchor};


    #[test]
    fn test_name_seg() {
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


    mod name_string {
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
            assert_parses!(NS::parse, b"\0",   b"",  NS::empty());
            assert_parses!(NS::parse, b"\0K",  b"K", NS::empty());
            assert_parses!(NS::parse, b"^^\0", b"",  NS::new_parent::<NameSeg>(2, &[]));
            assert_parses!(NS::parse, b"\\\0", b"",  NS::new_root::<NameSeg>(&[]));
        }

        #[test]
        fn test_parse_single() {
            let ns = &[b"A123"];
            assert_parses!(NS::parse, b"A123",    b"",  NS::new(ns));
            assert_parses!(NS::parse, b"A1234",   b"4", NS::new(ns));
            assert_parses!(NS::parse, b"^^^A123", b"",  NS::new_parent(3, ns));
            assert_parses!(NS::parse, b"\\A123",  b"",  NS::new_root(ns));
        }

        #[test]
        fn test_parse_dual() {
            let ns = &[b"A___", b"B___"];
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
            let a = b"A___";
            let b = b"B___";
            let c = b"C___";
            let d = b"D___";

            // Count = 0
            assert_parses!(NS::parse, b"\x2f\x00",     b"",     NS::empty());
            assert_parses!(NS::parse, b"\x2f\x00A___", b"A___", NS::empty());
            assert_parses!(NS::parse, b"^^\x2f\x00",   b"",     NS::new_parent::<NameSeg>(2, &[]));
            assert_parses!(NS::parse, b"\\\x2f\x00",   b"",     NS::new_root::<NameSeg>(&[]));

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


    #[test]
    fn test_path_anchor() {
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


mod data {
    use crate::acpi::aml::parse::Parse;
    use crate::acpi::aml::data::{Buffer, ComputationalData as CD};
    use crate::acpi::aml::misc::LocalObject;

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


mod package {
    use crate::acpi::aml::parse::package::*;
    use crate::acpi::aml::parse::state::ParserState;
    use nom::bytes::complete as bytes;

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
        let package_xyz = in_package(
            ParserState::lift(bytes::take_while(|b| b"XYZ".contains(&b))));
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
        let package_take_4 = in_package::<_, _, ()>(
            ParserState::lift(bytes::take(4_usize)));
        assert_errors!(&package_take_4, b"\x00");
        assert_errors!(&package_take_4, b"\x01A");
        assert_errors!(&package_take_4, b"\x02AB");
        assert_errors!(&package_take_4, b"\x03ABC");
        assert_parses!(&package_take_4, b"\x04ABCD",  b"",  b"ABCD");
        assert_parses!(&package_take_4, b"\x04ABCDE", b"E", b"ABCD");
        assert_errors!(&package_take_4, b"\x05ABCDE");  // Must consume whole package
    }
}


mod term {
    use crate::acpi::aml::parse::Parse;
    use crate::acpi::aml::term::*;
    use crate::acpi::aml::name::NameString;
    use crate::acpi::aml::data::{Buffer, ComputationalData};
    use crate::acpi::aml::misc::{ArgObject, LocalObject};
    use alloc::vec;


    mod named_object {
        use super::*;
        use super::NamedObject as N;

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
                region_name: NameString::empty(),
                bank_name: NameString::empty(),
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
                    bank_name: NameString::empty(),
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
                    name: NameString::empty(),
                }
            );
            assert_parses!(N::parse,
                b"\x8d\x6e\x0b\x3f\xec\\\x2eA___B___C___",
                b"C___",
                N::CreateBitField {
                    source_buffer: ArgObject::Arg6.into(),
                    bit_index: ComputationalData::Word(0xec3f).into(),
                    name: NameString::new_root(&[b"A___", b"B___"]),
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
                    name: NameString::empty(),
                }
            );
            assert_parses!(N::parse,
                b"\x8c\x6e\x0b\x3f\xec\\\x2eA___B___C___",
                b"C___",
                N::CreateByteField {
                    source_buffer: ArgObject::Arg6.into(),
                    byte_index: ComputationalData::Word(0xec3f).into(),
                    name: NameString::new_root(&[b"A___", b"B___"]),
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
                    name: NameString::empty(),
                }
            );
            assert_parses!(N::parse,
                b"\x8a\x6e\x0b\x3f\xec\\\x2eA___B___C___",
                b"C___",
                N::CreateDWordField {
                    source_buffer: ArgObject::Arg6.into(),
                    byte_index: ComputationalData::Word(0xec3f).into(),
                    name: NameString::new_root(&[b"A___", b"B___"]),
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
                    name: NameString::empty(),
                }
            );
            assert_parses!(N::parse,
                b"\x5b\x13\x6e\x0b\x3f\xec\x0a\x4c\\\x2eA___B___C___",
                b"C___",
                N::CreateField {
                    source_buffer: ArgObject::Arg6.into(),
                    bit_index: ComputationalData::Word(0xec3f).into(),
                    num_bits: ComputationalData::Byte(0x4c).into(),
                    name: NameString::new_root(&[b"A___", b"B___"]),
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
                    name: NameString::empty(),
                }
            );
            assert_parses!(N::parse,
                b"\x8f\x6e\x0b\x3f\xec\\\x2eA___B___C___",
                b"C___",
                N::CreateQWordField {
                    source_buffer: ArgObject::Arg6.into(),
                    byte_index: ComputationalData::Word(0xec3f).into(),
                    name: NameString::new_root(&[b"A___", b"B___"]),
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
                    name: NameString::empty(),
                }
            );
            assert_parses!(N::parse,
                b"\x8b\x6e\x0b\x3f\xec\\\x2eA___B___C___",
                b"C___",
                N::CreateWordField {
                    source_buffer: ArgObject::Arg6.into(),
                    byte_index: ComputationalData::Word(0xec3f).into(),
                    name: NameString::new_root(&[b"A___", b"B___"]),
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
                    name: NameString::empty(),
                    signature: ComputationalData::Zero.into(),
                    oem_id: ComputationalData::Zero.into(),
                    oem_table_id: ComputationalData::Zero.into(),
                }
            );
            assert_parses!(N::parse, b"\x5b\x88\\_311\x0a\x42\xff\x6a\x94", b"\x94",
                N::DataTableRegion {
                    name: NameString::new_root(&[b"_311"]),
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
                    name: NameString::empty(),
                    body: vec![],
                }
            );
            assert_parses!(N::parse,
                //        pkg name  body[0]             body[1]                 rest
                //        |   |     |                   |                       |
                b"\x5b\x82\x14^^ABCD\x8d\x63\x0a\x42_57Z\x11\x04\x0a\x3d\xf5\x83\x62\x01",
                b"\x62\x01",
                N::Device {
                    name: NameString::new_parent(2, &[b"ABCD"]),
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
            assert_parses!(N::parse, b"\x5b\x02\x00", b"", N::Event(NameString::empty()));
            assert_parses!(N::parse,
                b"\x5b\x02\\\x2f\x03A___B___C___D___",
                b"D___",
                N::Event(
                    NameString::new_root(&[
                        b"A___",
                        b"B___",
                        b"C___",
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
                name: NameString::empty(),
                object_type: ObjectType::Uninitialized,
                argument_count: 0,
            });
            assert_parses!(N::parse, b"\x15^_123\x10\x07\x94\x8b", b"\x94\x8b", N::External {
                name: NameString::new_parent(1, &[b"_123"]),
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
                region_name: NameString::empty(),
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
                    region_name: NameString::new_root(&[b"ABCD"]),
                    flags: FieldFlags {
                        access_type: AccessType::Buffer,
                        lock: true,
                        update_rule: UpdateRule::WriteAsOnes,
                    },
                    elements: vec![
                        FieldElement::Reserved { bit_length: 0x098f_d853 },
                        FieldElement::Named {
                            name: b"_B34".into(),
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
                index_name: NameString::empty(),
                data_name: NameString::empty(),
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
                    index_name: NameString::new_parent(1, &[b"_987"]),
                    data_name: NameString::new_root(&[b"ABCD"]),
                    flags: FieldFlags {
                        access_type: AccessType::Buffer,
                        lock: true,
                        update_rule: UpdateRule::WriteAsOnes,
                    },
                    elements: vec![
                        FieldElement::Reserved { bit_length: 0x098f_d853 },
                        FieldElement::Named {
                            name: b"_B34".into(),
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
                    name: NameString::empty(),
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
                    name: NameString::new_parent(2, &[b"Z49F"]),
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
                name: NameString::empty(),
                sync_level: 0,
            });
            assert_parses!(N::parse, b"\x5b\x01\\_BL8\x0f\x94\x7f", b"\x94\x7f", N::Mutex {
                name: NameString::new_root(&[b"_BL8"]),
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
                    name: NameString::empty(),
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
                    name: NameString::new_parent(1, &[b"_J8M"]),
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
                    name: NameString::empty(),
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
                    name: NameString::new_root(&[b"_487"]),
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
                    name: NameString::empty(),
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
                    name: NameString::new_parent(1, &[b"_842"]),
                    id: 0xf4,
                    register_block_addr: 0xed42_83a2,
                    register_block_length: 0xd2,
                    body: vec![
                        Buffer {
                            size: ComputationalData::Byte(0x3d).into(),
                            initializer: &[0xf5, 0x83],
                        }.into(),
                        N::Event(NameString::new(&[b"ABCD"])).into(),
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
                    name: NameString::empty(),
                    body: vec![],
                }
            );
            assert_parses!(N::parse,
                //        pkg name  body[0]                 body[1]     rest
                //        |   |     |                       |           |
                b"\x5b\x85\x11\\_F37\x11\x04\x0a\x3d\xf5\x83\x5b\x02ABCDEFGH",
                b"EFGH",
                N::ThermalZone {
                    name: NameString::new_root(&[b"_F37"]),
                    body: vec![
                        Buffer {
                            size: ComputationalData::Byte(0x3d).into(),
                            initializer: &[0xf5, 0x83],
                        }.into(),
                        N::Event(NameString::new(&[b"ABCD"])).into(),
                    ],
                }
            );
        }
    }


    #[test]
    fn test_field_flags() {
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


    mod field_element {
        use super::*;
        use super::FieldElement as F;

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
            let abcd = b"ABCD";
            let x123 = b"X123";

            assert_errors!(F::parse, b"\x02");
            assert_errors!(F::parse, b"\x02\xff");
            assert_parses!(F::parse,
                b"\x02\x00",
                b"",
                F::ConnectNamed(NameString::empty()),
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


    #[test]
    fn test_method_flags() {
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


mod misc {
    use crate::acpi::aml::parse::Parse;
    use crate::acpi::aml::misc::*;
    use ArgObject::*;

    #[test]
    fn test_arg_obj() {
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
