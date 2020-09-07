#![feature(custom_inner_attributes)]
#![rustfmt::skip]

use tartan_bitfield::{get_bit, get_bits, set_bit, set_bits};
use tartan_bitfield::bitfield;
use core::mem;

#[test]
fn test_get_bit() {
    for i in 0..8 {
        assert_eq!(get_bit(0x00_u8,   i), false);
        assert_eq!(get_bit(0xff_u8,   i), true);
        assert_eq!(get_bit(1_u8 << i, i), true);
    }

    for i in 0..16 {
        assert_eq!(get_bit(0_u16,      i), false);
        assert_eq!(get_bit(u16::MAX,   i), true);
        assert_eq!(get_bit(1_u16 << i, i), true);
    }

    for i in 0..32 {
        assert_eq!(get_bit(0_u32,      i), false);
        assert_eq!(get_bit(u32::MAX,   i), true);
        assert_eq!(get_bit(1_u32 << i, i), true);
    }

    for i in 0..64 {
        assert_eq!(get_bit(0_u64,      i), false);
        assert_eq!(get_bit(u64::MAX,   i), true);
        assert_eq!(get_bit(1_u64 << i, i), true);
    }

    for i in 0..128 {
        assert_eq!(get_bit(0_u128,      i), false);
        assert_eq!(get_bit(u128::MAX,   i), true);
        assert_eq!(get_bit(1_u128 << i, i), true);
    }

    assert_eq!(get_bit(0x0000_0001_u32,  0), true);
    assert_eq!(get_bit(0x0000_0001_u32,  1), false);
    assert_eq!(get_bit(0x0000_0001_u32, 17), false);
    assert_eq!(get_bit(0x0000_0001_u32, 31), false);

    assert_eq!(get_bit(0x8000_0000_u32,  0), false);
    assert_eq!(get_bit(0x8000_0000_u32, 17), false);
    assert_eq!(get_bit(0x8000_0000_u32, 30), false);
    assert_eq!(get_bit(0x8000_0000_u32, 31), true);

    assert_eq!(get_bit(0x0002_0000_u32,  0), false);
    assert_eq!(get_bit(0x0002_0000_u32, 16), false);
    assert_eq!(get_bit(0x0002_0000_u32, 17), true);
    assert_eq!(get_bit(0x0002_0000_u32, 18), false);
    assert_eq!(get_bit(0x0002_0000_u32, 31), false);

    assert_eq!(get_bit(0xffff_fffe_u32,  0), false);
    assert_eq!(get_bit(0xffff_fffe_u32,  1), true);
    assert_eq!(get_bit(0xffff_fffe_u32, 17), true);
    assert_eq!(get_bit(0xffff_fffe_u32, 31), true);

    assert_eq!(get_bit(0x7fff_ffff_u32,  0), true);
    assert_eq!(get_bit(0x7fff_ffff_u32, 17), true);
    assert_eq!(get_bit(0x7fff_ffff_u32, 30), true);
    assert_eq!(get_bit(0x7fff_ffff_u32, 31), false);

    assert_eq!(get_bit(0xa5a5_a5a5_u32,  0), true);
    assert_eq!(get_bit(0xa5a5_a5a5_u32,  1), false);
    assert_eq!(get_bit(0xa5a5_a5a5_u32, 16), true);
    assert_eq!(get_bit(0xa5a5_a5a5_u32, 17), false);
    assert_eq!(get_bit(0xa5a5_a5a5_u32, 18), true);
    assert_eq!(get_bit(0xa5a5_a5a5_u32, 30), false);
    assert_eq!(get_bit(0xa5a5_a5a5_u32, 31), true);
}

#[test]
#[should_panic]
fn test_get_bit_panic_u8_overflow() {
    let _ = get_bit(0_u8, 8);
}

#[test]
#[should_panic]
fn test_get_bit_panic_u128_overflow() {
    let _ = get_bit(0_u128, 128);
}

#[test]
fn test_set_bit() {
    // false => true
    assert_eq!(set_bit(0_u8,     0, true), 0x01);
    assert_eq!(set_bit(0_u8,     1, true), 0x02);
    assert_eq!(set_bit(0_u8,     2, true), 0x04);
    assert_eq!(set_bit(0_u8,     3, true), 0x08);
    assert_eq!(set_bit(0_u8,     4, true), 0x10);
    assert_eq!(set_bit(0_u8,     5, true), 0x20);
    assert_eq!(set_bit(0_u8,     6, true), 0x40);
    assert_eq!(set_bit(0_u8,     7, true), 0x80);
    assert_eq!(set_bit(0_u16,    0, true), 1);
    assert_eq!(set_bit(0_u32,    0, true), 1);
    assert_eq!(set_bit(0_u64,    0, true), 1);
    assert_eq!(set_bit(0_u128,   0, true), 1);
    assert_eq!(set_bit(0_u16,   15, true), 0x8000);
    assert_eq!(set_bit(0_u32,   31, true), 0x8000_0000);
    assert_eq!(set_bit(0_u64,   63, true), 0x8000_0000_0000_0000);
    assert_eq!(set_bit(0_u128, 127, true), 0x8000_0000_0000_0000_0000_0000_0000_0000);

    // false => false
    for i in 0..8 {
        assert_eq!(set_bit(0_u8, i, false), 0);
    }
    for i in 0..16 {
        assert_eq!(set_bit(0_u16, i, false), 0);
    }
    for i in 0..32 {
        assert_eq!(set_bit(0_u32, i, false), 0);
    }
    for i in 0..64 {
        assert_eq!(set_bit(0_u64, i, false), 0);
    }
    for i in 0..128 {
        assert_eq!(set_bit(0_u128, i, false), 0);
    }

    // true => false
    assert_eq!(set_bit(0xff_u8,     0, false), 0xfe);
    assert_eq!(set_bit(0xff_u8,     1, false), 0xfd);
    assert_eq!(set_bit(0xff_u8,     2, false), 0xfb);
    assert_eq!(set_bit(0xff_u8,     3, false), 0xf7);
    assert_eq!(set_bit(0xff_u8,     4, false), 0xef);
    assert_eq!(set_bit(0xff_u8,     5, false), 0xdf);
    assert_eq!(set_bit(0xff_u8,     6, false), 0xbf);
    assert_eq!(set_bit(0xff_u8,     7, false), 0x7f);
    assert_eq!(set_bit(u16::MAX,    0, false), 0xfffe);
    assert_eq!(set_bit(u32::MAX,    0, false), 0xffff_fffe);
    assert_eq!(set_bit(u64::MAX,    0, false), 0xffff_ffff_ffff_fffe);
    assert_eq!(set_bit(u128::MAX,   0, false), 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_fffe);
    assert_eq!(set_bit(u16::MAX,   15, false), 0x7fff);
    assert_eq!(set_bit(u32::MAX,   31, false), 0x7fff_ffff);
    assert_eq!(set_bit(u64::MAX,   63, false), 0x7fff_ffff_ffff_ffff);
    assert_eq!(set_bit(u128::MAX, 127, false), 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff);

    // true => true
    for i in 0..8 {
        let x = 0xff_u8;
        assert_eq!(set_bit(x, i, true), 0xff);
    }
    for i in 0..16 {
        let x = 0xffff_u16;
        assert_eq!(set_bit(x, i, true), 0xffff);
    }
    for i in 0..32 {
        let x = 0xffff_ffff_u32;
        assert_eq!(set_bit(x, i, true), 0xffff_ffff);
    }
    for i in 0..64 {
        let x = 0xffff_ffff_ffff_ffff_u64;
        assert_eq!(set_bit(x, i, true), 0xffff_ffff_ffff_ffff);
    }
    for i in 0..128 {
        let x = 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_u128;
        assert_eq!(set_bit(x, i, true), 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff);
    }
}

#[test]
#[should_panic]
fn test_set_bit_panic_u8_overflow() {
    let _ = set_bit(0_u8, 8, true);
}

#[test]
#[should_panic]
fn test_set_bit_panic_u128_overflow() {
    let _ = set_bit(0_u128, 128, true);
}

#[test]
fn test_get_bits() {
    for i in 0..=8 {
        assert_eq!(get_bits(0x00_u8, 0, i), 0);
        assert_eq!(get_bits(0x00_u8, i, 8), 0);
        assert_eq!(get_bits(0xff_u8, i, i), 0);
    }
    for i in 0..8 {
        assert_eq!(get_bits(0x00_u8, i, i + 1), 0);
        assert_eq!(get_bits(0x00_u8, i, i + 1), 0);
        assert_eq!(get_bits(0xff_u8, i, i + 1), 1);
        assert_eq!(get_bits(0x01_u8 << i, i, i + 1), 1);
        assert_eq!(get_bits(!(0x01_u8 << i), i, i + 1), 0);
    }
    for i in 0..7 {
        assert_eq!(get_bits(0x00_u8, i, i + 2), 0x00);
        assert_eq!(get_bits(0x00_u8, i, i + 2), 0x00);
        assert_eq!(get_bits(0xff_u8, i, i + 2), 0x03);
        assert_eq!(get_bits(0x03_u8 << i, i, i + 2), 0x03);
        assert_eq!(get_bits(!(0x03_u8 << i), i, i + 2), 0x00);
    }
    for i in 0..6 {
        assert_eq!(get_bits(0x00_u8, i, i + 3), 0x00);
        assert_eq!(get_bits(0xff_u8, i, i + 3), 0x07);
        assert_eq!(get_bits(0x07_u8 << i, i, i + 3), 0x07);
        assert_eq!(get_bits(!(0x07_u8 << i), i, i + 3), 0x00);
    }

    for i in 0..=128 {
        assert_eq!(get_bits(0_u128, 0, i), 0);
        assert_eq!(get_bits(0_u128, i, 128), 0);
        assert_eq!(get_bits(u128::MAX, i, i), 0);
    }
    for i in 0..128 {
        assert_eq!(get_bits(0_u128, i, i + 1), 0);
        assert_eq!(get_bits(u128::MAX, i, i + 1), 1);
        assert_eq!(get_bits(0x01_u128 << i, i, i + 1), 1);
        assert_eq!(get_bits(!(0x01_u128 << i), i, i + 1), 0);
    }
    for i in 0..127 {
        assert_eq!(get_bits(0_u128, i, i + 2), 0x00);
        assert_eq!(get_bits(u128::MAX, i, i + 2), 0x03);
        assert_eq!(get_bits(0x03_u128 << i, i, i + 2), 0x03);
        assert_eq!(get_bits(!(0x03_u128 << i), i, i + 2), 0x00);
    }
    for i in 0..126 {
        assert_eq!(get_bits(0_u128, i, i + 2), 0x00);
        assert_eq!(get_bits(u128::MAX, i, i + 3), 0x07);
        assert_eq!(get_bits(0x07_u128 << i, i, i + 3), 0x07);
        assert_eq!(get_bits(!(0x07_u128 << i), i, i + 3), 0x00);
    }
    for i in 0..113 {
        assert_eq!(get_bits(0_u128, i, i + 16), 0x0000);
        assert_eq!(get_bits(u128::MAX, i, i + 16), 0xffff);
        assert_eq!(get_bits(0xffff_u128 << i, i, i + 16), 0xffff);
        assert_eq!(get_bits(!(0xffff_u128 << i), i, i + 16), 0x0000);
    }


    assert_eq!(get_bits(0xff_u8, 0, 0), 0x00);
    assert_eq!(get_bits(0xff_u8, 0, 1), 0x01);
    assert_eq!(get_bits(0xff_u8, 0, 2), 0x03);
    assert_eq!(get_bits(0xff_u8, 0, 3), 0x07);
    assert_eq!(get_bits(0xff_u8, 0, 4), 0x0f);
    assert_eq!(get_bits(0xff_u8, 0, 5), 0x1f);
    assert_eq!(get_bits(0xff_u8, 0, 6), 0x3f);
    assert_eq!(get_bits(0xff_u8, 0, 7), 0x7f);
    assert_eq!(get_bits(0xff_u8, 0, 8), 0xff);

    assert_eq!(get_bits(0x00_u8, 0, 0), 0x00);
    assert_eq!(get_bits(0x01_u8, 0, 1), 0x01);
    assert_eq!(get_bits(0x03_u8, 0, 2), 0x03);
    assert_eq!(get_bits(0x07_u8, 0, 3), 0x07);
    assert_eq!(get_bits(0x0f_u8, 0, 4), 0x0f);
    assert_eq!(get_bits(0x1f_u8, 0, 5), 0x1f);
    assert_eq!(get_bits(0x3f_u8, 0, 6), 0x3f);
    assert_eq!(get_bits(0x7f_u8, 0, 7), 0x7f);
    assert_eq!(get_bits(0xff_u8, 0, 8), 0xff);

    assert_eq!(get_bits(0xff_u8, 0, 0), 0);
    assert_eq!(get_bits(0xfe_u8, 0, 1), 0);
    assert_eq!(get_bits(0xfc_u8, 0, 2), 0);
    assert_eq!(get_bits(0xf8_u8, 0, 3), 0);
    assert_eq!(get_bits(0xf0_u8, 0, 4), 0);
    assert_eq!(get_bits(0xe0_u8, 0, 5), 0);
    assert_eq!(get_bits(0xc0_u8, 0, 6), 0);
    assert_eq!(get_bits(0x80_u8, 0, 7), 0);
    assert_eq!(get_bits(0x00_u8, 0, 8), 0);

    assert_eq!(get_bits(0xff_u8, 0, 8), 0xff);
    assert_eq!(get_bits(0xff_u8, 1, 8), 0x7f);
    assert_eq!(get_bits(0xff_u8, 2, 8), 0x3f);
    assert_eq!(get_bits(0xff_u8, 3, 8), 0x1f);
    assert_eq!(get_bits(0xff_u8, 4, 8), 0x0f);
    assert_eq!(get_bits(0xff_u8, 5, 8), 0x07);
    assert_eq!(get_bits(0xff_u8, 6, 8), 0x03);
    assert_eq!(get_bits(0xff_u8, 7, 8), 0x01);
    assert_eq!(get_bits(0xff_u8, 8, 8), 0x00);

    assert_eq!(get_bits(0xff_u8, 0, 8), 0xff);
    assert_eq!(get_bits(0xfe_u8, 1, 8), 0x7f);
    assert_eq!(get_bits(0xfc_u8, 2, 8), 0x3f);
    assert_eq!(get_bits(0xf8_u8, 3, 8), 0x1f);
    assert_eq!(get_bits(0xf0_u8, 4, 8), 0x0f);
    assert_eq!(get_bits(0xe0_u8, 5, 8), 0x07);
    assert_eq!(get_bits(0xc0_u8, 6, 8), 0x03);
    assert_eq!(get_bits(0x80_u8, 7, 8), 0x01);
    assert_eq!(get_bits(0x00_u8, 8, 8), 0x00);


    let n = 0xe781_2496_c35a_db69_u64;

    assert_eq!(get_bits(n,  0,  4), 0x9);
    assert_eq!(get_bits(n,  1,  5), 0x4);
    assert_eq!(get_bits(n,  2,  6), 0xa);

    assert_eq!(get_bits(n, 12, 16), 0xd);
    assert_eq!(get_bits(n, 13, 17), 0x6);
    assert_eq!(get_bits(n, 14, 18), 0xb);
    assert_eq!(get_bits(n, 15, 19), 0x5);
    assert_eq!(get_bits(n, 16, 20), 0xa);

    assert_eq!(get_bits(n, 28, 32), 0xc);
    assert_eq!(get_bits(n, 29, 33), 0x6);
    assert_eq!(get_bits(n, 30, 34), 0xb);
    assert_eq!(get_bits(n, 31, 35), 0xd);
    assert_eq!(get_bits(n, 32, 36), 0x6);

    assert_eq!(get_bits(n, 44, 48), 0x2);
    assert_eq!(get_bits(n, 45, 49), 0x9);
    assert_eq!(get_bits(n, 46, 50), 0x4);
    assert_eq!(get_bits(n, 47, 51), 0x2);
    assert_eq!(get_bits(n, 48, 52), 0x1);

    assert_eq!(get_bits(n, 58, 62), 0x9);
    assert_eq!(get_bits(n, 59, 63), 0xc);
    assert_eq!(get_bits(n, 60, 64), 0xe);

    assert_eq!(get_bits(n, 14, 46), 0x925b_0d6b);
    assert_eq!(get_bits(n, 15, 47), 0x492d_86b5);
    assert_eq!(get_bits(n, 16, 48), 0x2496_c35a);
    assert_eq!(get_bits(n, 17, 49), 0x924b_61ad);
    assert_eq!(get_bits(n, 18, 50), 0x4925_b0d6);
}

#[test]
fn test_set_bits() {
    for i in 0..=8 {
        assert_eq!(set_bits(0_u8, 0, i, 0), 0);
        assert_eq!(set_bits(0_u8, i, 8, 0), 0);

        assert_eq!(set_bits(0x00_u8, i, i, 0), 0x00);
        assert_eq!(set_bits(0xff_u8, i, i, 0), 0xff);
        assert_eq!(set_bits(0xa5_u8, i, i, 0), 0xa5);
    }
    for i in 0..8 {
        assert_eq!(set_bits(0_u8,      i, i + 1, 0), 0);
        assert_eq!(set_bits(0_u8,      i, i + 1, 1), 1 << i);
        assert_eq!(set_bits(0xff_u8,   i, i + 1, 0), !(1 << i));
        assert_eq!(set_bits(0xff_u8,   i, i + 1, 1), 0xff);
        assert_eq!(set_bits(1_u8 << i, i, i + 1, 0), 0);
    }
    for i in 0..7 {
        assert_eq!(set_bits(0_u8,      i, i + 2, 0), 0);
        assert_eq!(set_bits(0_u8,      i, i + 2, 3), 3 << i);
        assert_eq!(set_bits(0xff_u8,   i, i + 2, 0), !(3 << i));
        assert_eq!(set_bits(0xff_u8,   i, i + 2, 3), 0xff);
        assert_eq!(set_bits(3_u8 << i, i, i + 2, 0), 0);
    }
    for i in 0..6 {
        assert_eq!(set_bits(0_u8,      i, i + 3, 0), 0);
        assert_eq!(set_bits(0_u8,      i, i + 3, 7), 7 << i);
        assert_eq!(set_bits(0xff_u8,   i, i + 3, 0), !(7 << i));
        assert_eq!(set_bits(0xff_u8,   i, i + 3, 7), 0xff);
        assert_eq!(set_bits(7_u8 << i, i, i + 3, 0), 0);
    }

    for i in 0..=128 {
        assert_eq!(set_bits(0_u128, 0,   i, 0), 0);
        assert_eq!(set_bits(0_u128, i, 128, 0), 0);

        assert_eq!(set_bits(0_u128,     i, i, 0), 0);
        assert_eq!(set_bits(u128::MAX,  i, i, 0), u128::MAX);
        let nontrivial = 0xe781_2496_c35a_db69_0123_4567_89ab_cdef_u128;
        assert_eq!(set_bits(nontrivial, i, i, 0), nontrivial);
    }
    for i in 0..128 {
        assert_eq!(set_bits(0_u128,      i, i + 1, 0), 0);
        assert_eq!(set_bits(0_u128,      i, i + 1, 1), 1 << i);
        assert_eq!(set_bits(u128::MAX,   i, i + 1, 0), !(1 << i));
        assert_eq!(set_bits(u128::MAX,   i, i + 1, 1), u128::MAX);
        assert_eq!(set_bits(1_u128 << i, i, i + 1, 0), 0);
    }
    for i in 0..127 {
        assert_eq!(set_bits(0_u128,      i, i + 2, 0), 0);
        assert_eq!(set_bits(0_u128,      i, i + 2, 3), 3 << i);
        assert_eq!(set_bits(u128::MAX,   i, i + 2, 0), !(3 << i));
        assert_eq!(set_bits(u128::MAX,   i, i + 2, 3), u128::MAX);
        assert_eq!(set_bits(3_u128 << i, i, i + 2, 0), 0);
    }
    for i in 0..126 {
        assert_eq!(set_bits(0_u128,      i, i + 3, 0), 0);
        assert_eq!(set_bits(0_u128,      i, i + 3, 7), 7 << i);
        assert_eq!(set_bits(u128::MAX,   i, i + 3, 0), !(7 << i));
        assert_eq!(set_bits(u128::MAX,   i, i + 3, 7), u128::MAX);
        assert_eq!(set_bits(7_u128 << i, i, i + 3, 0), 0);
    }


    let n = 0xc35a_db69_u128;

    let zero = 0_u128;
    assert_eq!(set_bits(zero,  0,  32, n), 0x0000_0000_0000_0000_0000_0000_c35a_db69);
    assert_eq!(set_bits(zero,  1,  33, n), 0x0000_0000_0000_0000_0000_0001_86b5_b6d2);
    assert_eq!(set_bits(zero,  2,  34, n), 0x0000_0000_0000_0000_0000_0003_0d6b_6da4);
    assert_eq!(set_bits(zero, 40,  72, n), 0x0000_0000_0000_00c3_5adb_6900_0000_0000);
    assert_eq!(set_bits(zero, 41,  73, n), 0x0000_0000_0000_0186_b5b6_d200_0000_0000);
    assert_eq!(set_bits(zero, 42,  74, n), 0x0000_0000_0000_030d_6b6d_a400_0000_0000);
    assert_eq!(set_bits(zero, 43,  75, n), 0x0000_0000_0000_061a_d6db_4800_0000_0000);
    assert_eq!(set_bits(zero, 44,  76, n), 0x0000_0000_0000_0c35_adb6_9000_0000_0000);
    assert_eq!(set_bits(zero, 93, 125, n), 0x186b_5b6d_2000_0000_0000_0000_0000_0000);
    assert_eq!(set_bits(zero, 94, 126, n), 0x30d6_b6da_4000_0000_0000_0000_0000_0000);
    assert_eq!(set_bits(zero, 95, 127, n), 0x61ad_6db4_8000_0000_0000_0000_0000_0000);
    assert_eq!(set_bits(zero, 96, 128, n), 0xc35a_db69_0000_0000_0000_0000_0000_0000);

    let ones = u128::MAX;
    assert_eq!(set_bits(ones,  0,  32, n), 0xffff_ffff_ffff_ffff_ffff_ffff_c35a_db69);
    assert_eq!(set_bits(ones,  1,  33, n), 0xffff_ffff_ffff_ffff_ffff_ffff_86b5_b6d3);
    assert_eq!(set_bits(ones,  2,  34, n), 0xffff_ffff_ffff_ffff_ffff_ffff_0d6b_6da7);
    assert_eq!(set_bits(ones, 40,  72, n), 0xffff_ffff_ffff_ffc3_5adb_69ff_ffff_ffff);
    assert_eq!(set_bits(ones, 41,  73, n), 0xffff_ffff_ffff_ff86_b5b6_d3ff_ffff_ffff);
    assert_eq!(set_bits(ones, 42,  74, n), 0xffff_ffff_ffff_ff0d_6b6d_a7ff_ffff_ffff);
    assert_eq!(set_bits(ones, 43,  75, n), 0xffff_ffff_ffff_fe1a_d6db_4fff_ffff_ffff);
    assert_eq!(set_bits(ones, 44,  76, n), 0xffff_ffff_ffff_fc35_adb6_9fff_ffff_ffff);
    assert_eq!(set_bits(ones, 93, 125, n), 0xf86b_5b6d_3fff_ffff_ffff_ffff_ffff_ffff);
    assert_eq!(set_bits(ones, 94, 126, n), 0xf0d6_b6da_7fff_ffff_ffff_ffff_ffff_ffff);
    assert_eq!(set_bits(ones, 95, 127, n), 0xe1ad_6db4_ffff_ffff_ffff_ffff_ffff_ffff);
    assert_eq!(set_bits(ones, 96, 128, n), 0xc35a_db69_ffff_ffff_ffff_ffff_ffff_ffff);
}

bitfield! {
    pub struct BasicBitfieldTest(u32) {
        [24..32] pub a: u8,  // 8b, contains MSB
        // 16..24 skipped, but some covered by z below
        [11..16] pub b: u8,  // 5b
        [ 6..11] pub c: u8,  // 5b, overlaps byte boundary
        // 3..6 skipped entirely
        [ 2    ] pub d,      // single bit (bool)
        [ 0.. 2] pub e: u8,  // 2b, contains LSB
        [10..20] pub z: u16, // 10b, overlaps with a & b
    }
}

#[test]
fn test_bitfield_basic() {
    let mut x = BasicBitfieldTest(0);
    assert_eq!(x.a(), 0);
    assert_eq!(x.b(), 0);
    assert_eq!(x.c(), 0);
    assert_eq!(x.d(), false);
    assert_eq!(x.e(), 0);

    x.set_a(0xff);
    x.set_b(0x1f);
    x.set_c(0x1f);
    x.set_d(true);
    x.set_e(0x03);
    assert_eq!(x.0, 0xff00_ffc7);

    let mut y = BasicBitfieldTest(0xffff_ffff);
    assert_eq!(y.a(), 0xff);
    assert_eq!(y.b(), 0x1f);
    assert_eq!(y.c(), 0x1f);
    assert_eq!(y.d(), true);
    assert_eq!(y.e(), 0x03);

    y.set_a(0);
    y.set_b(0);
    y.set_c(0);
    y.set_d(false);
    y.set_e(0);
    assert_eq!(y.0, 0x00ff_0038);

    // Reserved/skipped bits still matter for equality
    assert_ne!(x, BasicBitfieldTest(0xffff_ffff));
    assert_eq!(x, BasicBitfieldTest(0xff00_ffc7));
    assert_ne!(y, BasicBitfieldTest(0x0000_0000));
    assert_eq!(y, BasicBitfieldTest(0x00ff_0038));
}

#[test]
fn test_bitfield_conversions() {
    let examples: &[u32] = &[
        0x0000_0000,
        0xffff_ffff,
        0xc35a_db69,
        0x0123_4567,
        0x89ab_cdef,
        0xa5a5_a5a5,
    ];

    for inner_orig in examples.iter() {
        // Unsafe blocks below should be safe thanks to #[repr(transparent)]

        // Convert from underlying representation to bitfield struct
        let struct_a = BasicBitfieldTest(*inner_orig);
        let struct_b = BasicBitfieldTest::from(*inner_orig);
        let struct_c: BasicBitfieldTest = (*inner_orig).into();
        let struct_d: BasicBitfieldTest = unsafe { mem::transmute(*inner_orig) };
        let struct_e = unsafe {
            *(inner_orig as *const u32 as *const BasicBitfieldTest)
        };
        assert_eq!(struct_a, struct_b);
        assert_eq!(struct_a, struct_c);
        assert_eq!(struct_a, struct_d);
        assert_eq!(struct_a, struct_e);

        // Convert from bitfield struct to underlying representation
        let inner_a = struct_a.0;
        let inner_b = u32::from(struct_a);
        let inner_c: u32 = struct_a.into();
        let inner_d: u32 = unsafe { mem::transmute(struct_a) };
        let inner_e = unsafe {
            *(&struct_a as *const BasicBitfieldTest as *const u32)
        };
        assert_eq!(*inner_orig, inner_a);
        assert_eq!(*inner_orig, inner_b);
        assert_eq!(*inner_orig, inner_c);
        assert_eq!(*inner_orig, inner_d);
        assert_eq!(*inner_orig, inner_e);
    }
}
