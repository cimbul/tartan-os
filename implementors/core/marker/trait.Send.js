(function() {var implementors = {};
implementors["aml_parse"] = [{"text":"impl Send for UsageError","synthetic":true,"types":[]}];
implementors["tartan_acpi"] = [{"text":"impl Send for RootDescriptionPointerV1","synthetic":true,"types":[]},{"text":"impl Send for RootDescriptionPointerV2","synthetic":true,"types":[]},{"text":"impl Send for DescriptionHeader","synthetic":true,"types":[]},{"text":"impl Send for FixedDescription","synthetic":true,"types":[]},{"text":"impl Send for GenericAddress","synthetic":true,"types":[]},{"text":"impl Send for AddressSpace","synthetic":true,"types":[]},{"text":"impl Send for AccessSize","synthetic":true,"types":[]},{"text":"impl Send for FixedFlags","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Send for AMLTable&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Send for MethodSignature","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Send for ParserState&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Send for NameSeg","synthetic":true,"types":[]},{"text":"impl Send for NameString","synthetic":true,"types":[]},{"text":"impl Send for PathAnchor","synthetic":true,"types":[]},{"text":"impl Send for SimpleName","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Send for SuperName&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Send for Buffer&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Send for Package&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Send for VarPackage&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Send for ComputationalData&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Send for PackageElement&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Send for DataObject&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Send for DataRefObject&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Send for FieldFlags","synthetic":true,"types":[]},{"text":"impl Send for MethodFlags","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Send for TermObject&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Send for TermArg&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Send for NameSpaceModifier&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Send for NamedObject&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Send for AccessType","synthetic":true,"types":[]},{"text":"impl Send for UpdateRule","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Send for FieldElement&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Send for AccessAttrib","synthetic":true,"types":[]},{"text":"impl Send for RegionSpace","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Send for StatementOpcode&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Send for ReferenceExpressionOpcode&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Send for ExpressionOpcode&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Send for MatchOpcode","synthetic":true,"types":[]},{"text":"impl Send for ObjectType","synthetic":true,"types":[]},{"text":"impl Send for DebugObject","synthetic":true,"types":[]},{"text":"impl Send for ArgObject","synthetic":true,"types":[]},{"text":"impl Send for LocalObject","synthetic":true,"types":[]}];
implementors["tartan_arch"] = [{"text":"impl Send for BasicTaskStateSegment","synthetic":true,"types":[]},{"text":"impl Send for TaskStateSegmentHeader","synthetic":true,"types":[]},{"text":"impl Send for PrivilegedStack","synthetic":true,"types":[]},{"text":"impl&lt;T:&nbsp;?Sized&gt; Send for TaskStateSegmentBitmaps&lt;T&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;T: Send,&nbsp;</span>","synthetic":true,"types":[]},{"text":"impl Send for ControlRegister8","synthetic":true,"types":[]},{"text":"impl Send for ExtendedFeatureEnableRegister","synthetic":true,"types":[]},{"text":"impl Send for TaskStateSegmentHeader","synthetic":true,"types":[]},{"text":"impl Send for FlagRegister","synthetic":true,"types":[]},{"text":"impl Send for ControlRegister0","synthetic":true,"types":[]},{"text":"impl Send for ControlRegister4","synthetic":true,"types":[]},{"text":"impl Send for ExtendedControlRegister0","synthetic":true,"types":[]},{"text":"impl Send for BasicFeatures","synthetic":true,"types":[]},{"text":"impl Send for ExtendedFeatures","synthetic":true,"types":[]},{"text":"impl Send for AddressSpaceSizes","synthetic":true,"types":[]},{"text":"impl Send for InterruptVector","synthetic":true,"types":[]},{"text":"impl Send for InterruptDescriptorTableRegister","synthetic":true,"types":[]},{"text":"impl Send for APICBaseRegister","synthetic":true,"types":[]},{"text":"impl Send for ControlRegister2","synthetic":true,"types":[]},{"text":"impl Send for ControlRegister3","synthetic":true,"types":[]},{"text":"impl Send for Level2PageTableEntry","synthetic":true,"types":[]},{"text":"impl Send for Level1PageTableEntry","synthetic":true,"types":[]},{"text":"impl Send for GlobalDescriptorTableRegister","synthetic":true,"types":[]},{"text":"impl Send for Selector","synthetic":true,"types":[]},{"text":"impl Send for ApplicationDescriptorFlags","synthetic":true,"types":[]},{"text":"impl Send for SystemDescriptorType","synthetic":true,"types":[]},{"text":"impl Send for GenericDescriptor","synthetic":true,"types":[]},{"text":"impl Send for GenericDescriptorFlags","synthetic":true,"types":[]},{"text":"impl Send for SegmentDescriptor","synthetic":true,"types":[]},{"text":"impl Send for SegmentDescriptorFlags","synthetic":true,"types":[]},{"text":"impl Send for GateDescriptor","synthetic":true,"types":[]},{"text":"impl Send for GateDescriptorFlags","synthetic":true,"types":[]},{"text":"impl&lt;T:&nbsp;?Sized&gt; Send for IOPermissionBitmap&lt;T&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;T: Send,&nbsp;</span>","synthetic":true,"types":[]},{"text":"impl Send for LocalDescriptorTableRegister","synthetic":true,"types":[]},{"text":"impl Send for TaskRegister","synthetic":true,"types":[]},{"text":"impl Send for SegmentRegister","synthetic":true,"types":[]}];
implementors["tartan_devicetree"] = [{"text":"impl&lt;'a&gt; Send for Value&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Send for MemoryReservation","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Send for Tree&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Send for StructureData&lt;'a&gt;","synthetic":true,"types":[]}];
implementors["tartan_elf"] = [{"text":"impl&lt;Addr&gt; Send for Header&lt;Addr&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;Addr: Send,&nbsp;</span>","synthetic":true,"types":[]},{"text":"impl Send for HeaderIdent","synthetic":true,"types":[]},{"text":"impl Send for FileClass","synthetic":true,"types":[]},{"text":"impl Send for Endianness","synthetic":true,"types":[]},{"text":"impl Send for OSABI","synthetic":true,"types":[]},{"text":"impl Send for FileType","synthetic":true,"types":[]},{"text":"impl Send for Machine","synthetic":true,"types":[]},{"text":"impl Send for ProgramHeader32","synthetic":true,"types":[]},{"text":"impl Send for ProgramHeader64","synthetic":true,"types":[]},{"text":"impl Send for ProgramSegmentType","synthetic":true,"types":[]},{"text":"impl Send for ProgramSegmentFlags","synthetic":true,"types":[]},{"text":"impl&lt;Addr&gt; Send for SectionHeader&lt;Addr&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;Addr: Send,&nbsp;</span>","synthetic":true,"types":[]},{"text":"impl Send for SectionType","synthetic":true,"types":[]},{"text":"impl Send for SectionFlags","synthetic":true,"types":[]}];
implementors["tartan_kernel"] = [{"text":"impl&lt;'a&gt; Send for Allocator&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Send for BlockList&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Send for CursorMut&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Send for BlockHeader","synthetic":true,"types":[]}];
implementors["tartan_parsers"] = [{"text":"impl&lt;'a, I&gt; Send for Position&lt;'a, I&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;I: Sync,&nbsp;</span>","synthetic":true,"types":[]},{"text":"impl&lt;'a, I&gt; Send for ErrorWithPosition&lt;'a, I&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;I: Send,&nbsp;</span>","synthetic":true,"types":[]}];
implementors["tartan_pci"] = [{"text":"impl Send for CapabilityEntry","synthetic":true,"types":[]},{"text":"impl Send for ConfigSelector","synthetic":true,"types":[]},{"text":"impl Send for MemMapConfigAccess","synthetic":true,"types":[]},{"text":"impl Send for IOConfigAccess","synthetic":true,"types":[]},{"text":"impl Send for HeaderRegister0","synthetic":true,"types":[]},{"text":"impl Send for HeaderRegister1","synthetic":true,"types":[]},{"text":"impl Send for HeaderRegister2","synthetic":true,"types":[]},{"text":"impl Send for HeaderRegister3","synthetic":true,"types":[]},{"text":"impl Send for Type0HeaderRegister11","synthetic":true,"types":[]},{"text":"impl Send for Type0HeaderRegister13","synthetic":true,"types":[]},{"text":"impl Send for Type0HeaderRegister15","synthetic":true,"types":[]},{"text":"impl Send for CommandRegister","synthetic":true,"types":[]},{"text":"impl Send for StatusRegister","synthetic":true,"types":[]},{"text":"impl Send for SelfTest","synthetic":true,"types":[]},{"text":"impl Send for BaseAddressRegister","synthetic":true,"types":[]},{"text":"impl Send for GenericCapabilityRegister","synthetic":true,"types":[]},{"text":"impl Send for AddressSpace","synthetic":true,"types":[]},{"text":"impl Send for AddressWidth","synthetic":true,"types":[]}];
implementors["tartan_serial"] = [{"text":"impl&lt;T&gt; Send for UARTWriteAdapter&lt;T&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;T: Send,&nbsp;</span>","synthetic":true,"types":[]},{"text":"impl Send for LineMode","synthetic":true,"types":[]},{"text":"impl Send for NullUART","synthetic":true,"types":[]},{"text":"impl Send for Parity","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Send for UARTPL011&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Send for RegisterBlock","synthetic":true,"types":[]},{"text":"impl Send for ActualRegisterAccess","synthetic":true,"types":[]},{"text":"impl&lt;A&gt; Send for UART16550&lt;A&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;A: Send,&nbsp;</span>","synthetic":true,"types":[]}];
implementors["tartan_uefi"] = [{"text":"impl Send for Handle","synthetic":true,"types":[]},{"text":"impl Send for Revision","synthetic":true,"types":[]},{"text":"impl Send for Status","synthetic":true,"types":[]},{"text":"impl Send for TableHeader","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; !Send for SystemTable&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Send for RuntimeServices","synthetic":true,"types":[]},{"text":"impl Send for BootServices","synthetic":true,"types":[]},{"text":"impl Send for GUID","synthetic":true,"types":[]},{"text":"impl Send for ConfigurationTable","synthetic":true,"types":[]},{"text":"impl Send for MemoryDescriptor","synthetic":true,"types":[]},{"text":"impl Send for MemoryMap","synthetic":true,"types":[]},{"text":"impl Send for MemoryType","synthetic":true,"types":[]},{"text":"impl Send for AllocateType","synthetic":true,"types":[]},{"text":"impl Send for MemoryAttributes","synthetic":true,"types":[]},{"text":"impl Send for OpenProtocolAttributes","synthetic":true,"types":[]},{"text":"impl Send for BootAllocator","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Send for OutputStream&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Send for Logger&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Send for SimpleTextInput","synthetic":true,"types":[]},{"text":"impl Send for SimpleTextOutput","synthetic":true,"types":[]},{"text":"impl Send for SimpleTextOutputMode","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; !Send for LoadedImage&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Send for SimpleFileSystem","synthetic":true,"types":[]},{"text":"impl Send for File","synthetic":true,"types":[]},{"text":"impl Send for FileMode","synthetic":true,"types":[]},{"text":"impl Send for FileAttributes","synthetic":true,"types":[]}];
if (window.register_implementors) {window.register_implementors(implementors);} else {window.pending_implementors = implementors;}})()