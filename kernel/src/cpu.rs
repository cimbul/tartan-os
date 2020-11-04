use core::fmt;


#[cfg(not(any(target_arch = "x86", target_arch = "x86_64")))]
pub fn print_state(_: &mut dyn fmt::Write) -> fmt::Result {
    // TODO
    Ok(())
}

#[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
pub fn print_state(out: &mut dyn fmt::Write) -> fmt::Result {
    use tartan_arch::x86_common::{self, features, interrupt, paging, protection};

    writeln!(out, "CPUID max basic: {:x?}", features::max_cpuid_index_basic())?;
    writeln!(out, "CPUID max ext.: {:x?}", features::max_cpuid_index_extended())?;

    let basic_features = features::BasicFeatures::get();
    writeln!(out, "{:#x?}", basic_features)?;
    writeln!(out, "{:#x?}", features::ExtendedFeatures::get())?;
    writeln!(out, "{:#x?}", features::AddressSpaceSizes::get())?;

    writeln!(out, "{:#x?}", x86_common::FlagRegister::get())?;
    writeln!(out, "{:#x?}", x86_common::ControlRegister0::get())?;
    writeln!(out, "{:#x?}", x86_common::ControlRegister4::get())?;

    writeln!(out, "{:#x?}", paging::ControlRegister2::get())?;
    writeln!(out, "{:#x?}", paging::ControlRegister3::get())?;

    #[allow(clippy::if_not_else)]
    if !basic_features.extended_state_save() {
        writeln!(out, "ExtendedControlRegister0 unsupported")?;
    } else if !basic_features.extended_state_save_enabled() {
        writeln!(out, "ExtendedControlRegister0 disabled")?;
    } else {
        writeln!(out, "{:#x?}", x86_common::ExtendedControlRegister0::get())?;
    }

    #[cfg(target_arch = "x86_64")]
    {
        use tartan_arch::x86_64;

        writeln!(out, "{:#x?}", x86_64::ControlRegister8::get())?;
        writeln!(out, "{:#x?}", x86_64::ExtendedFeatureEnableRegister::get())?;
    }

    writeln!(out, "{:#x?}", interrupt::InterruptDescriptorTableRegister::get())?;
    writeln!(out, "{:#x?}", protection::GlobalDescriptorTableRegister::get())?;
    describe_segment_register(
        out,
        "LDTR",
        protection::LocalDescriptorTableRegister::get(),
    )?;
    describe_segment_register(out, "TR", protection::TaskRegister::get())?;
    describe_segment_register(out, "CS", protection::SegmentRegister::Code.get())?;
    describe_segment_register(out, "DS", protection::SegmentRegister::Data.get())?;
    describe_segment_register(out, "SS", protection::SegmentRegister::Stack.get())?;
    describe_segment_register(out, "ES", protection::SegmentRegister::Extra.get())?;
    describe_segment_register(out, "FS", protection::SegmentRegister::ExtraF.get())?;
    describe_segment_register(out, "GS", protection::SegmentRegister::ExtraG.get())?;

    Ok(())
}

#[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
fn describe_segment_register(
    out: &mut dyn fmt::Write,
    name: &str,
    selector: tartan_arch::x86_common::protection::Selector,
) -> fmt::Result {
    use tartan_arch::x86_common::protection::{
        DescriptorFlags, GateDescriptor, GenericDescriptor, SegmentDescriptor,
    };

    writeln!(out)?;
    writeln!(out, "{}:", name)?;
    writeln!(out, "{:#x?}", selector)?;

    let descriptor = selector.descriptor_address() as *const GenericDescriptor;
    let descriptor_flags = unsafe { (*descriptor).flags };
    if descriptor_flags.is_gate() {
        let gate_descriptor = unsafe { &*(descriptor as *const GateDescriptor) };
        writeln!(out, "{:#x?}", *gate_descriptor)?;
    } else {
        let seg_descriptor = unsafe { &*(descriptor as *const SegmentDescriptor) };
        writeln!(out, "{:#x?}", *seg_descriptor)?;
    }

    Ok(())
}
