use core::fmt;
use tartan_pci as pci;
use tartan_pci::access::{ConfigAccess, ConfigSelector};

#[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
pub fn find_pci_access() -> impl ConfigAccess {
    tartan_pci::access::io::IOConfigAccess
}

#[cfg(target_arch = "arm")]
pub fn find_pci_access() -> impl ConfigAccess {
    // FIXME: Hardcoded values copied from QEMU's "virt" board device tree
    tartan_pci::access::MemMapConfigAccess {
        base_address: 0x3f00_0000,
        bus_range: 0x00..=0x0f,
    }
}

#[cfg(target_arch = "aarch64")]
pub fn find_pci_access() -> impl ConfigAccess {
    // FIXME: Hardcoded values copied from QEMU's "virt" board device tree
    tartan_pci::access::MemMapConfigAccess {
        base_address: 0x0000_0040_1000_0000,
        bus_range: 0x00..=0xff,
    }
}

pub fn print_devices(out: &mut dyn fmt::Write) -> fmt::Result {
    writeln!(out, "Enumerating PCI devices on bus 0")?;
    let access = find_pci_access();
    for selector in pci::enumerate_bus(&access, ConfigSelector::default()) {
        let id_register: pci::config::HeaderRegister0 =
            access.get_fixed_register(selector);

        writeln!(
            out,
            " {:2x}:{:x}: vendor {:04x} device {:04x}",
            selector.device,
            selector.function,
            id_register.vendor(),
            id_register.device(),
        )?;

        let register_3: pci::config::HeaderRegister3 =
            access.get_fixed_register(selector);
        let function_count_note = if selector.function != 0 {
            "" // Does not apply when this is not the first function
        } else if register_3.multi_function() {
            " (multi-function)"
        } else {
            " (single-function)"
        };
        writeln!(
            out,
            "       header type {:02x}{}",
            register_3.header_type(),
            function_count_note,
        )?;


        let class_register: pci::config::HeaderRegister2 =
            access.get_fixed_register(selector);
        writeln!(
            out,
            "       class {:02x} subclass {:02x} interface {:02x} revision {:02x}",
            class_register.class(),
            class_register.subclass(),
            class_register.interface(),
            class_register.revision(),
        )?;

        for capability in pci::iter_capabilities(&access, selector) {
            writeln!(
                out,
                "       capability {:02x} (register {:04x})",
                capability.id, capability.register,
            )?;
        }
    }

    Ok(())
}
