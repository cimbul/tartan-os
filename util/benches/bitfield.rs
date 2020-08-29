#![feature(custom_test_frameworks)]
#![test_runner(criterion::runner)]

use core::convert::From;
use criterion::{
    black_box, measurement::Measurement, BenchmarkGroup, BenchmarkId, Criterion,
};
use criterion_macro::criterion;
use tartan_util::bitfield;


fn benchmark_group<F, M>(c: &mut Criterion<M>, s: &str, f: F)
where
    F: FnOnce(&mut BenchmarkGroup<'_, M>),
    M: Measurement,
{
    let mut g = c.benchmark_group(s);
    f(&mut g);
    g.finish();
}


#[criterion]
fn bench_nothing(c: &mut Criterion) {
    // We do a lot of __micro__ micro benchmarking here. It's useful to have a reference
    // for how much overhead comes from Criterion itself.
    c.bench_function("do nothing", |b| b.iter(|| black_box(42)));
}


#[criterion]
fn bench_set_bit(c: &mut Criterion) {
    // This is a macro instead of a function to avoid declaring a dozen traits for
    // different integer sizes, and also allow the caller to select which values to
    // black_box()
    macro_rules! bench_set_bit_with_size {
        [
            $size:ty,
            $g:expr,
            $value:expr,
            $bit_num:expr $(,)?
        ] => {
            {
                let g = $g;

                for &bit_value in [false, true].iter() {
                    g.bench_with_input(
                        BenchmarkId::new("actual", bit_value),
                        &bit_value,
                        |b, _| b.iter(||
                            bitfield::set_bit($value, $bit_num, black_box(bit_value))
                        ),
                    );
                }

                for &bit_value in [false, true].iter() {
                    g.bench_with_input(
                        BenchmarkId::new("manual", bit_value),
                        &bit_value,
                        |b, _| b.iter(|| {
                            let mut x = $value;
                            let i = $bit_num;
                            x &= !(1 << i);
                            x |= <$size>::from(black_box(bit_value)) << i;
                            x
                        }),
                    );
                }

                // This performs the best, but that's almost certainly because of branch
                // prediction. That is applicable to many real-world cases, but not all.
                for &bit_value in [false, true].iter() {
                    g.bench_with_input(
                        BenchmarkId::new("manual branched", bit_value),
                        &bit_value,
                        |b, _| b.iter(|| {
                            let mut x = $value;
                            let i = $bit_num;
                            if black_box(bit_value) {
                                x |= 1 << i;
                            } else {
                                x &= !(1 << i);
                            }
                            x
                        })
                    );
                }
            }
        };
    }


    benchmark_group(c, "set_bit::<u128>", |g| {
        bench_set_bit_with_size!(u128, g, black_box(0_u128), black_box(79));
    });

    benchmark_group(c, "set_bit::<u128> inline bit num", |g| {
        bench_set_bit_with_size!(u128, g, black_box(0_u128), 79);
    });

    benchmark_group(c, "set_bit::<u64> inline bit num", |g| {
        bench_set_bit_with_size!(u64, g, black_box(0_u64), 42);
    });
}


#[criterion]
fn bench_bitfield(c: &mut Criterion) {
    bitfield! {
        struct ExampleBitfield(u32) {
            [29    ] flag,
            [25..29] unaligned: u8,
            [16..24] aligned_byte: u8,
            [ 4..16] multi_byte: u16,
        }
    }

    #[repr(C, packed)]
    #[derive(Clone, Copy)]
    struct ExamplePacked {
        upper: u8,
        aligned_byte: u8,
        multi_byte: u16,
    }

    let raw = 0xcf83_1a35_u32;
    let bitfield = ExampleBitfield(raw);
    let packed = ExamplePacked { upper: 0xcf, aligned_byte: 0x83, multi_byte: 0x1a35 };

    benchmark_group(c, "get aligned byte", |g| {
        g.bench_function("actual", |b| b.iter(|| black_box(bitfield).aligned_byte()));
        g.bench_function("packed struct", |b| b.iter(|| black_box(packed).aligned_byte));
        g.bench_function("manual", |b| b.iter(|| (black_box(raw) >> 16) as u8));
    });

    benchmark_group(c, "set aligned byte", |g| {
        g.bench_function("actual", |b| {
            b.iter(|| {
                let mut x = black_box(bitfield);
                x.set_aligned_byte(black_box(0x5a));
                x
            })
        });

        g.bench_function("packed struct", |b| {
            b.iter(|| {
                let mut x = black_box(packed);
                x.aligned_byte = black_box(0x5a);
                x
            })
        });

        g.bench_function("manual", |b| {
            b.iter(|| {
                black_box(raw) & 0xff00_ffff | (u32::from(black_box(0x5a_u8)) << 16)
            })
        });
    });

    benchmark_group(c, "get unaligned field", |g| {
        g.bench_function("actual", |b| b.iter(|| black_box(bitfield).unaligned()));

        g.bench_function("packed struct", |b| {
            b.iter(|| ((black_box(packed).upper >> 1) & 0xf) as u8)
        });

        g.bench_function("manual", |b| b.iter(|| ((black_box(raw) >> 25) & 0xf) as u8));
    });

    benchmark_group(c, "set unaligned field", |g| {
        g.bench_function("actual", |b| {
            b.iter(|| {
                let mut x = black_box(bitfield);
                x.set_unaligned(black_box(0xa));
                x
            })
        });

        g.bench_function("packed struct", |b| {
            b.iter(|| {
                let mut x = black_box(packed);
                x.upper &= 0xe1;
                x.upper |= (black_box(0xa) & 0xf) << 1;
                x
            })
        });

        g.bench_function("manual", |b| {
            b.iter(|| {
                black_box(raw) & 0xe1ff_ffff | (u32::from(black_box(0xa_u8)) & 0xf) << 25
            })
        });
    });

    benchmark_group(c, "get flag", |g| {
        g.bench_function("actual", |b| b.iter(|| black_box(bitfield).flag()));

        g.bench_function("packed struct", |b| {
            b.iter(|| (black_box(packed).upper >> 5) & 1 != 0)
        });

        g.bench_function("manual", |b| b.iter(|| (black_box(raw) >> 29) & 1 != 0));
    });

    benchmark_group(c, "set flag", |g| {
        g.bench_function("actual", |b| {
            b.iter(|| {
                let mut x = black_box(bitfield);
                x.set_flag(black_box(true));
                x
            })
        });

        g.bench_function("packed struct", |b| {
            b.iter(|| {
                let mut x = black_box(packed);
                x.upper &= 0xdf;
                x.upper |= u8::from(black_box(true)) << 1;
                x
            })
        });

        g.bench_function("manual", |b| {
            b.iter(|| black_box(raw) & 0xdfff_ffff | u32::from(black_box(true)) << 29)
        });
    });

    benchmark_group(c, "get multi-byte field", |g| {
        g.bench_function("actual", |b| b.iter(|| black_box(bitfield).multi_byte()));

        g.bench_function("packed struct", |b| {
            b.iter(|| ((black_box(packed).multi_byte >> 4) & 0x0fff) as u16)
        });

        g.bench_function("manual", |b| {
            b.iter(|| ((black_box(raw) >> 4) & 0x0fff) as u16)
        });
    });

    benchmark_group(c, "set multi-byte field", |g| {
        g.bench_function("actual", |b| {
            b.iter(|| {
                let mut x = black_box(bitfield);
                x.set_multi_byte(black_box(0xe1b));
                x
            })
        });

        g.bench_function("packed struct", |b| {
            b.iter(|| {
                let mut x = black_box(packed);
                x.multi_byte &= 0x000f;
                x.multi_byte |= black_box(0xe1b) << 4;
                x
            })
        });

        g.bench_function("manual", |b| {
            b.iter(|| {
                black_box(raw) & 0xffff_000f
                    | (u32::from(black_box(0xe1b_u16)) & 0x0fff) << 4
            })
        });
    });
}
