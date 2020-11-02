(function() {var implementors = {};
implementors["aml_parse"] = [{"text":"impl Freeze for UsageError","synthetic":true,"types":[]}];
implementors["tartan_acpi"] = [{"text":"impl Freeze for RootDescriptionPointerV1","synthetic":true,"types":[]},{"text":"impl Freeze for RootDescriptionPointerV2","synthetic":true,"types":[]},{"text":"impl Freeze for DescriptionHeader","synthetic":true,"types":[]},{"text":"impl Freeze for FixedDescription","synthetic":true,"types":[]},{"text":"impl Freeze for GenericAddress","synthetic":true,"types":[]},{"text":"impl Freeze for AddressSpace","synthetic":true,"types":[]},{"text":"impl Freeze for AccessSize","synthetic":true,"types":[]},{"text":"impl Freeze for FixedFlags","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Freeze for AMLTable&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Freeze for MethodSignature","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Freeze for ParserState&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Freeze for NameSeg","synthetic":true,"types":[]},{"text":"impl Freeze for NameString","synthetic":true,"types":[]},{"text":"impl Freeze for PathAnchor","synthetic":true,"types":[]},{"text":"impl Freeze for SimpleName","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Freeze for SuperName&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Freeze for Buffer&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Freeze for Package&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Freeze for VarPackage&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Freeze for ComputationalData&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Freeze for PackageElement&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Freeze for DataObject&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Freeze for DataRefObject&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Freeze for FieldFlags","synthetic":true,"types":[]},{"text":"impl Freeze for MethodFlags","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Freeze for TermObject&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Freeze for TermArg&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Freeze for NameSpaceModifier&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Freeze for NamedObject&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Freeze for AccessType","synthetic":true,"types":[]},{"text":"impl Freeze for UpdateRule","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Freeze for FieldElement&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Freeze for AccessAttrib","synthetic":true,"types":[]},{"text":"impl Freeze for RegionSpace","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Freeze for StatementOpcode&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Freeze for ReferenceExpressionOpcode&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Freeze for ExpressionOpcode&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Freeze for MatchOpcode","synthetic":true,"types":[]},{"text":"impl Freeze for ObjectType","synthetic":true,"types":[]},{"text":"impl Freeze for DebugObject","synthetic":true,"types":[]},{"text":"impl Freeze for ArgObject","synthetic":true,"types":[]},{"text":"impl Freeze for LocalObject","synthetic":true,"types":[]}];
implementors["tartan_arch"] = [{"text":"impl Freeze for BasicTaskStateSegment","synthetic":true,"types":[]},{"text":"impl Freeze for TaskStateSegmentHeader","synthetic":true,"types":[]},{"text":"impl Freeze for PrivilegedStack","synthetic":true,"types":[]},{"text":"impl&lt;T:&nbsp;?Sized&gt; Freeze for TaskStateSegmentBitmaps&lt;T&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;T: Freeze,&nbsp;</span>","synthetic":true,"types":[]},{"text":"impl Freeze for ControlRegister8","synthetic":true,"types":[]},{"text":"impl Freeze for ExtendedFeatureEnableRegister","synthetic":true,"types":[]},{"text":"impl Freeze for TaskStateSegmentHeader","synthetic":true,"types":[]},{"text":"impl Freeze for FlagRegister","synthetic":true,"types":[]},{"text":"impl Freeze for ControlRegister0","synthetic":true,"types":[]},{"text":"impl Freeze for ControlRegister4","synthetic":true,"types":[]},{"text":"impl Freeze for ExtendedControlRegister0","synthetic":true,"types":[]},{"text":"impl Freeze for BasicFeatures","synthetic":true,"types":[]},{"text":"impl Freeze for ExtendedFeatures","synthetic":true,"types":[]},{"text":"impl Freeze for AddressSpaceSizes","synthetic":true,"types":[]},{"text":"impl Freeze for InterruptVector","synthetic":true,"types":[]},{"text":"impl Freeze for InterruptDescriptorTableRegister","synthetic":true,"types":[]},{"text":"impl Freeze for APICBaseRegister","synthetic":true,"types":[]},{"text":"impl Freeze for ControlRegister2","synthetic":true,"types":[]},{"text":"impl Freeze for ControlRegister3","synthetic":true,"types":[]},{"text":"impl Freeze for Level2PageTableEntry","synthetic":true,"types":[]},{"text":"impl Freeze for Level1PageTableEntry","synthetic":true,"types":[]},{"text":"impl Freeze for GlobalDescriptorTableRegister","synthetic":true,"types":[]},{"text":"impl Freeze for Selector","synthetic":true,"types":[]},{"text":"impl Freeze for ApplicationDescriptorFlags","synthetic":true,"types":[]},{"text":"impl Freeze for SystemDescriptorType","synthetic":true,"types":[]},{"text":"impl Freeze for GenericDescriptor","synthetic":true,"types":[]},{"text":"impl Freeze for GenericDescriptorFlags","synthetic":true,"types":[]},{"text":"impl Freeze for SegmentDescriptor","synthetic":true,"types":[]},{"text":"impl Freeze for SegmentDescriptorFlags","synthetic":true,"types":[]},{"text":"impl Freeze for GateDescriptor","synthetic":true,"types":[]},{"text":"impl Freeze for GateDescriptorFlags","synthetic":true,"types":[]},{"text":"impl&lt;T:&nbsp;?Sized&gt; Freeze for IOPermissionBitmap&lt;T&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;T: Freeze,&nbsp;</span>","synthetic":true,"types":[]},{"text":"impl Freeze for LocalDescriptorTableRegister","synthetic":true,"types":[]},{"text":"impl Freeze for TaskRegister","synthetic":true,"types":[]},{"text":"impl Freeze for SegmentRegister","synthetic":true,"types":[]}];
implementors["tartan_devicetree"] = [{"text":"impl&lt;'a&gt; Freeze for Value&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Freeze for MemoryReservation","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Freeze for Tree&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Freeze for StructureData&lt;'a&gt;","synthetic":true,"types":[]}];
implementors["tartan_elf"] = [{"text":"impl&lt;Addr&gt; Freeze for Header&lt;Addr&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;Addr: Freeze,&nbsp;</span>","synthetic":true,"types":[]},{"text":"impl Freeze for HeaderIdent","synthetic":true,"types":[]},{"text":"impl Freeze for FileClass","synthetic":true,"types":[]},{"text":"impl Freeze for Endianness","synthetic":true,"types":[]},{"text":"impl Freeze for OSABI","synthetic":true,"types":[]},{"text":"impl Freeze for FileType","synthetic":true,"types":[]},{"text":"impl Freeze for Machine","synthetic":true,"types":[]},{"text":"impl Freeze for ProgramHeader32","synthetic":true,"types":[]},{"text":"impl Freeze for ProgramHeader64","synthetic":true,"types":[]},{"text":"impl Freeze for ProgramSegmentType","synthetic":true,"types":[]},{"text":"impl Freeze for ProgramSegmentFlags","synthetic":true,"types":[]},{"text":"impl&lt;Addr&gt; Freeze for SectionHeader&lt;Addr&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;Addr: Freeze,&nbsp;</span>","synthetic":true,"types":[]},{"text":"impl Freeze for SectionType","synthetic":true,"types":[]},{"text":"impl Freeze for SectionFlags","synthetic":true,"types":[]}];
implementors["tartan_parsers"] = [{"text":"impl&lt;'a, I&gt; Freeze for Position&lt;'a, I&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a, I&gt; Freeze for ErrorWithPosition&lt;'a, I&gt;","synthetic":true,"types":[]}];
implementors["tartan_pci"] = [{"text":"impl Freeze for CapabilityEntry","synthetic":true,"types":[]},{"text":"impl Freeze for ConfigSelector","synthetic":true,"types":[]},{"text":"impl Freeze for MemMapConfigAccess","synthetic":true,"types":[]},{"text":"impl Freeze for IOConfigAccess","synthetic":true,"types":[]},{"text":"impl Freeze for HeaderRegister0","synthetic":true,"types":[]},{"text":"impl Freeze for HeaderRegister1","synthetic":true,"types":[]},{"text":"impl Freeze for HeaderRegister2","synthetic":true,"types":[]},{"text":"impl Freeze for HeaderRegister3","synthetic":true,"types":[]},{"text":"impl Freeze for Type0HeaderRegister11","synthetic":true,"types":[]},{"text":"impl Freeze for Type0HeaderRegister13","synthetic":true,"types":[]},{"text":"impl Freeze for Type0HeaderRegister15","synthetic":true,"types":[]},{"text":"impl Freeze for CommandRegister","synthetic":true,"types":[]},{"text":"impl Freeze for StatusRegister","synthetic":true,"types":[]},{"text":"impl Freeze for SelfTest","synthetic":true,"types":[]},{"text":"impl Freeze for BaseAddressRegister","synthetic":true,"types":[]},{"text":"impl Freeze for GenericCapabilityRegister","synthetic":true,"types":[]},{"text":"impl Freeze for AddressSpace","synthetic":true,"types":[]},{"text":"impl Freeze for AddressWidth","synthetic":true,"types":[]}];
implementors["tartan_serial"] = [{"text":"impl Freeze for LineMode","synthetic":true,"types":[]},{"text":"impl Freeze for NullUART","synthetic":true,"types":[]},{"text":"impl Freeze for Parity","synthetic":true,"types":[]},{"text":"impl Freeze for ActualRegisterAccess","synthetic":true,"types":[]},{"text":"impl&lt;A&gt; Freeze for UART16550&lt;A&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;A: Freeze,&nbsp;</span>","synthetic":true,"types":[]}];
implementors["tartan_uefi"] = [{"text":"impl Freeze for Handle","synthetic":true,"types":[]},{"text":"impl Freeze for Revision","synthetic":true,"types":[]},{"text":"impl Freeze for Status","synthetic":true,"types":[]},{"text":"impl Freeze for TableHeader","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Freeze for SystemTable&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Freeze for RuntimeServices","synthetic":true,"types":[]},{"text":"impl Freeze for BootServices","synthetic":true,"types":[]},{"text":"impl Freeze for GUID","synthetic":true,"types":[]},{"text":"impl Freeze for ConfigurationTable","synthetic":true,"types":[]},{"text":"impl Freeze for MemoryDescriptor","synthetic":true,"types":[]},{"text":"impl Freeze for MemoryMap","synthetic":true,"types":[]},{"text":"impl Freeze for MemoryType","synthetic":true,"types":[]},{"text":"impl Freeze for AllocateType","synthetic":true,"types":[]},{"text":"impl Freeze for MemoryAttributes","synthetic":true,"types":[]},{"text":"impl Freeze for OpenProtocolAttributes","synthetic":true,"types":[]},{"text":"impl Freeze for BootAllocator","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Freeze for OutputStream&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Freeze for Logger&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Freeze for SimpleTextInput","synthetic":true,"types":[]},{"text":"impl Freeze for SimpleTextOutput","synthetic":true,"types":[]},{"text":"impl Freeze for SimpleTextOutputMode","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Freeze for LoadedImage&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Freeze for SimpleFileSystem","synthetic":true,"types":[]},{"text":"impl Freeze for File","synthetic":true,"types":[]},{"text":"impl Freeze for FileMode","synthetic":true,"types":[]},{"text":"impl Freeze for FileAttributes","synthetic":true,"types":[]}];
if (window.register_implementors) {window.register_implementors(implementors);} else {window.pending_implementors = implementors;}})()