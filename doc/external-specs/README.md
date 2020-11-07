# External specifications

Links to some of the specifications relevant to Tartan OS are listed below. This folder is
a convenient place to store them. Other than this README, nothing here will be checked
into Git, because many of these documents don't allow free (libre) redistribution.

**See also**: LLVM's [Architecture & Platform Information for Compiler
Writers](https://llvm.org/docs/CompilerWriterInfo.html), which covers much of the same
ground.


## Processor manuals

### x86/x86-64

  * [Intel 64 and IA-32 Architecture Software Developer's Manuals](https://software.intel.com/content/www/us/en/develop/articles/intel-sdm.html)
    * [Volume 1: Basic Architecture](https://software.intel.com/content/www/us/en/develop/download/intel-64-and-ia-32-architectures-software-developers-manual-volume-1-basic-architecture.html),
      May 2020
    * [Volume 2: Instruction Set Reference](https://software.intel.com/content/www/us/en/develop/download/intel-64-and-ia-32-architectures-sdm-combined-volumes-2a-2b-2c-and-2d-instruction-set-reference-a-z.html),
      May 2020
    * [Volume 3: System Programming Guide](https://software.intel.com/content/www/us/en/develop/download/intel-64-and-ia-32-architectures-sdm-combined-volumes-3a-3b-3c-and-3d-system-programming-guide.html),
      May 2020
    * [Volume 4: Model-Specific Registers](https://software.intel.com/content/www/us/en/develop/download/intel-64-and-ia-32-architectures-software-developers-manual-volume-4-model-specific-registers.html),
      May 2020
  * [AMD64 Architecture Programmer's Manual](https://developer.amd.com/resources/developer-guides-manuals/)
    * [Volume 1: Application Programming](http://support.amd.com/TechDocs/24592.pdf),
      Dec 2017
    * [Volume 2: System Programming](http://support.amd.com/TechDocs/24593.pdf),
      May 2020
    * [Volume 3: General-Purpose and System Instructions](http://support.amd.com/TechDocs/24594.pdf),
      Apr 2020
    * [Volume 4: 128/256-bit Media Instructions](http://support.amd.com/TechDocs/26568.pdf),
      May 2020
    * [Volume 5: 64-bit Media and x87 Floating-Point Instructions](https://www.amd.com/system/files/TechDocs/26569_APM_V5.pdf),
      May 2018

### Arm

  * [Armv8-A Architecture Reference Manual](https://developer.arm.com/documentation/ddi0487/latest)
    vF.c, Jul 2020
  * [Armv8-R Architecture Reference Manual](https://developer.arm.com/documentation/ddi0568/latest)
    vA.b, Mar 2017 (supplement to A profile manual)
  * [Armv8-M Architecture Reference Manual](https://developer.arm.com/documentation/ddi0553/latest)
    vB.l, Jun 2020
  * [Armv7-A/R Architecture Reference Manual](https://developer.arm.com/documentation/ddi0406/latest)
    vC.d, Mar 2018
  * [Armv7-M Architecture Reference Manual](https://developer.arm.com/documentation/ddi0403/latest)
    vE.d, Jun 2018
  * [Armv6-M Architecture Reference Manual](https://developer.arm.com/documentation/ddi0419/latest)
    vE, Jun 2018
  * [Arm Architecture Reference Manual](https://developer.arm.com/documentation/ddi0100/latest)
    vI, Jul 2005 (covers up to v6/ARM11 prior to v6-M/Cortex)
      * [Arm Architecture Reference Manual Thumb-2 Supplement](https://developer.arm.com/documentation/ddi0308/latest)
        vD, Dec 2005
  * [Arm and Thumb-2 Instruction Set Quick Reference](https://developer.arm.com/documentation/qrc0001/latest)
    vM, Sep 2008
  * [Thumb 16-bit Instruction Set Quick Reference](https://developer.arm.com/documentation/qrc0006/latest)
    vE, Sep 2008
  * [Arm Compiler armasm User Guide](https://developer.arm.com/documentation/dui0801/)
    vK (6.6.4), Aug 2020


## Firmwmare interfaces

  * [Unified Extensible Firmware Interface](https://uefi.org/specifications) (UEFI) v2.8,
    May 2020
  * [Advanced Configuration and Power Interface](https://uefi.org/specifications) (ACPI)
    v6.3, Jan 2019
    * [Older ACPI specifications](https://uefi.org/acpi/specs) before v6.0
    * [ACPI for Arm Components](https://developer.arm.com/documentation/den0093/latest)
      v1.0, Jul 2020
  * [Devicetree Specification](https://www.devicetree.org/specifications/) v0.3, Feb 2020

### Arm Base Systems

  * [Arm Server Base System Architecture](https://developer.arm.com/documentation/den0029/latest)
    (SBSA) v6.0 (0029D), Jun 2020
  * [Arm Server Base Boot Requirements](https://developer.arm.com/documentation/den0044/latest)
    (SBBR) v1.2 (E), Jun 2020
  * [Arm Embedded Base Boot Requirements](https://github.com/ARM-software/ebbr/releases)
    (EBBR) v1.0.1, Aug 2020


## Buses and host controllers

### Peripheral Component Interconnect (PCI), including PCI Express (PCIe)

  * [PCI Local Bus Specification](https://www.ics.uci.edu/~harris/ics216/pci/PCI_22.pdf)
    v2.2, Dec 1998
  * [PCI Local Bus Specification](https://lekensteyn.nl/files/docs/PCI_SPEV_V3_0.pdf)
    v3.0, Feb 2004
  * [PCI Express Base Specification](https://www.intel.com/content/dam/altera-www/global/en_US/uploads/e/e2/PCI_Express_Base_r2.1.pdf)
    v2.01, Mar 2009
  * [PCI Firmware Specification](http://read.pudn.com/downloads211/doc/comm/994029/pcifw_r3_0_updated.pdf)
    v3.0, Jun 2005 (x86/x86-64/IA-64 for PCI/PCI-X/PCIe)

### Universal Serial Bus (USB)

  * [USB4 Specification](https://www.usb.org/document-library/usb4tm-specification),
    Jun 2020
  * [USB 3.2 Specification](https://www.usb.org/document-library/usb-32-specification-released-september-22-2017-and-ecns)
    v1.0, Sep 2017 (Spec), Jul 2020 (ECNs)
    * [eXtensible Host Controller Interface](https://www.intel.com/content/www/us/en/products/docs/io/universal-serial-bus/extensible-host-controler-interface-usb-xhci.html)
      (xHCI) v1.9, May 2019
  * [USB 2.0 Specification](https://www.usb.org/document-library/usb-20-specification)
    v2.0, Apr 2000 (Spec), May 2019 (ECNs)
    * [Enhanced Host Controller Interface](https://www.intel.com/content/www/us/en/products/docs/io/universal-serial-bus/ehci-specification-for-usb.html)
      (EHCI) v1.0, Mar 2002
  * Universal Host Controller Interface (UHCI) v1.1, Mar 1996

    <ftp://ftp.netbsd.org/pub/NetBSD/misc/blymn/uhci11d.pdf>
  * [Open Host Controller Interface](https://www.getusb.info/resources/hcir1_0a.pdf)
    (OHCI) v1.0a, Aug 1999


## ABIs

### SysV ABI, including Executable and Linker Format (ELF)

Architecture-agnostic core defintion (gABI):
  * SCO [System V ABI](http://www.sco.com/developers/devspecs/)
    v4.1, Mar 1997
    ([PDF](http://www.sco.com/developers/devspecs/gabi41.pdf))
  * SCO [System V ABI Update](http://www.sco.com/developers/gabi/latest/contents.html),
    Jun 2013

Architecture-specific extensions (psABI):
  * x86:
    * SCO [Intel386 Architecture Processor Supplement](http://www.sco.com/developers/devspecs/)
      Fourth Edition, Mar 1997
      ([PDF](http://www.sco.com/developers/devspecs/abi386-4.pdf))
    * Lu, et al. [Intel386 Architecture Processor Supplement](https://raw.githubusercontent.com/wiki/hjl-tools/x86-psABI/intel386-psABI-1.1.pdf)
      v1.1, Dec 2015
    * Lu, et al. [latest sources](https://gitlab.com/x86-psABIs/i386-ABI)
  * x86-64:
    * [AMD64 Architecture Processor Supplement](https://raw.githubusercontent.com/wiki/hjl-tools/x86-psABI/x86-64-psABI-1.0.pdf)
      v1.0, Jan 2018
    * Lu, et al. [latest sources](https://gitlab.com/x86-psABIs/x86-64-ABI)
  * Arm (32-bit):
    * [ELF for the Arm Architecture](https://developer.arm.com/documentation/ihi0044/latest)
      v2019Q1
  * Arm (64-bit):
    * [ELF for the Arm 64-bit Architecture](https://developer.arm.com/documentation/ihi0056/latest)
      v2020Q2

### Arm ABI

  * [All Arm ABI documents](https://developer.arm.com/architectures/system-architectures/software-standards/abi)
  * 32-bit:
    * [Arm ABI Base](https://developer.arm.com/documentation/ihi0036/latest)
      (BSABI) v2019Q4
    * [Procedure Call Standard](https://developer.arm.com/documentation/ihi0042/latest)
      (AAPCS) v2020Q2
    * [Exception Handling ABI](https://developer.arm.com/documentation/ihi0038/latest)
      (EHABI) vB (2.10), Nov 2015
      [Run-time ABI](https://developer.arm.com/documentation/ihi0043/latest)
      (RTABI) vD (2.09), Nov 2012
  * 64-bit:
    * [Procedure Call Standard](https://github.com/ARM-software/abi-aa/releases)
      (AAPCS64) v2020Q2

### Exception handling

  * [LLVM Exception Handling](https://llvm.org/docs/ExceptionHandling.html)
  * [Itanium C++ ABI: Exception Handling](http://itanium-cxx-abi.github.io/cxx-abi/abi-eh.html)
    v1.22, Mar 2005
  * [DWARF Debugging Information Format](http://dwarfstd.org/doc/DWARF5.pdf)
    v5, Feb 2017
  * [Linux Standard Base Core, Generic: §10.6 Exception Frames](https://refspecs.linuxfoundation.org/LSB_5.0.0/LSB-Core-generic/LSB-Core-generic/ehframechpt.html)
    v5.0, Jun 2015


## Devices

  * [USB Human Interface Devices](https://www.usb.org/hid)
    * [Device Class Definition for HID 1.11](https://www.usb.org/document-library/device-class-definition-hid-111)
      v1.11, Jun 2001
  * [PrimeCell UART (PL011) Technical Reference Manual](https://developer.arm.com/documentation/ddi0183/latest)
    vG, Dec 2017
