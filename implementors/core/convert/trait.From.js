(function() {var implementors = {};
implementors["tartan_acpi"] = [{"text":"impl&lt;'a&gt; From&lt;&amp;'a [u8]&gt; for ParserState&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl From&lt;&amp;'_ [u8; 4]&gt; for NameSeg","synthetic":false,"types":[]},{"text":"impl From&lt;&amp;'_ [u8; 4]&gt; for NameString","synthetic":false,"types":[]},{"text":"impl From&lt;NameSeg&gt; for NameString","synthetic":false,"types":[]},{"text":"impl From&lt;NameString&gt; for SimpleName","synthetic":false,"types":[]},{"text":"impl From&lt;ArgObject&gt; for SimpleName","synthetic":false,"types":[]},{"text":"impl From&lt;LocalObject&gt; for SimpleName","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; From&lt;SimpleName&gt; for SuperName&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; From&lt;NameString&gt; for SuperName&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; From&lt;ArgObject&gt; for SuperName&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; From&lt;LocalObject&gt; for SuperName&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; From&lt;ReferenceExpressionOpcode&lt;'a&gt;&gt; for SuperName&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; From&lt;DataRefObject&lt;'a&gt;&gt; for PackageElement&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; From&lt;NameString&gt; for PackageElement&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; From&lt;ComputationalData&lt;'a&gt;&gt; for DataObject&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; From&lt;Package&lt;'a&gt;&gt; for DataObject&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; From&lt;VarPackage&lt;'a&gt;&gt; for DataObject&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; From&lt;DataObject&lt;'a&gt;&gt; for DataRefObject&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; From&lt;ComputationalData&lt;'a&gt;&gt; for DataRefObject&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; From&lt;Package&lt;'a&gt;&gt; for DataRefObject&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; From&lt;VarPackage&lt;'a&gt;&gt; for DataRefObject&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; From&lt;NameSpaceModifier&lt;'a&gt;&gt; for TermObject&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; From&lt;NamedObject&lt;'a&gt;&gt; for TermObject&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; From&lt;StatementOpcode&lt;'a&gt;&gt; for TermObject&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; From&lt;ExpressionOpcode&lt;'a&gt;&gt; for TermObject&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; From&lt;ReferenceExpressionOpcode&lt;'a&gt;&gt; for TermObject&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; From&lt;Buffer&lt;'a&gt;&gt; for TermObject&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; From&lt;Package&lt;'a&gt;&gt; for TermObject&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; From&lt;VarPackage&lt;'a&gt;&gt; for TermObject&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; From&lt;ExpressionOpcode&lt;'a&gt;&gt; for TermArg&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; From&lt;ReferenceExpressionOpcode&lt;'a&gt;&gt; for TermArg&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; From&lt;Buffer&lt;'a&gt;&gt; for TermArg&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; From&lt;Package&lt;'a&gt;&gt; for TermArg&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; From&lt;VarPackage&lt;'a&gt;&gt; for TermArg&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; From&lt;DataObject&lt;'a&gt;&gt; for TermArg&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; From&lt;ComputationalData&lt;'a&gt;&gt; for TermArg&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; From&lt;ArgObject&gt; for TermArg&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; From&lt;LocalObject&gt; for TermArg&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; From&lt;NameString&gt; for TermArg&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; From&lt;ReferenceExpressionOpcode&lt;'a&gt;&gt; for ExpressionOpcode&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; From&lt;Buffer&lt;'a&gt;&gt; for ExpressionOpcode&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; From&lt;Package&lt;'a&gt;&gt; for ExpressionOpcode&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; From&lt;VarPackage&lt;'a&gt;&gt; for ExpressionOpcode&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl From&lt;u8&gt; for AddressSpace","synthetic":false,"types":[]},{"text":"impl From&lt;AddressSpace&gt; for u8","synthetic":false,"types":[]},{"text":"impl From&lt;u8&gt; for AccessSize","synthetic":false,"types":[]},{"text":"impl From&lt;AccessSize&gt; for u8","synthetic":false,"types":[]},{"text":"impl From&lt;u32&gt; for FixedFlags","synthetic":false,"types":[]},{"text":"impl From&lt;FixedFlags&gt; for u32","synthetic":false,"types":[]}];
implementors["tartan_arch"] = [{"text":"impl From&lt;usize&gt; for ControlRegister8","synthetic":false,"types":[]},{"text":"impl From&lt;ControlRegister8&gt; for usize","synthetic":false,"types":[]},{"text":"impl From&lt;u64&gt; for ExtendedFeatureEnableRegister","synthetic":false,"types":[]},{"text":"impl From&lt;ExtendedFeatureEnableRegister&gt; for u64","synthetic":false,"types":[]},{"text":"impl From&lt;u64&gt; for BasicFeatures","synthetic":false,"types":[]},{"text":"impl From&lt;BasicFeatures&gt; for u64","synthetic":false,"types":[]},{"text":"impl From&lt;u64&gt; for ExtendedFeatures","synthetic":false,"types":[]},{"text":"impl From&lt;ExtendedFeatures&gt; for u64","synthetic":false,"types":[]},{"text":"impl From&lt;u32&gt; for AddressSpaceSizes","synthetic":false,"types":[]},{"text":"impl From&lt;AddressSpaceSizes&gt; for u32","synthetic":false,"types":[]},{"text":"impl From&lt;u8&gt; for InterruptVector","synthetic":false,"types":[]},{"text":"impl From&lt;InterruptVector&gt; for u8","synthetic":false,"types":[]},{"text":"impl From&lt;u64&gt; for APICBaseRegister","synthetic":false,"types":[]},{"text":"impl From&lt;APICBaseRegister&gt; for u64","synthetic":false,"types":[]},{"text":"impl From&lt;usize&gt; for ControlRegister2","synthetic":false,"types":[]},{"text":"impl From&lt;ControlRegister2&gt; for usize","synthetic":false,"types":[]},{"text":"impl From&lt;usize&gt; for ControlRegister3","synthetic":false,"types":[]},{"text":"impl From&lt;ControlRegister3&gt; for usize","synthetic":false,"types":[]},{"text":"impl From&lt;usize&gt; for Level2PageTableEntry","synthetic":false,"types":[]},{"text":"impl From&lt;Level2PageTableEntry&gt; for usize","synthetic":false,"types":[]},{"text":"impl From&lt;usize&gt; for Level1PageTableEntry","synthetic":false,"types":[]},{"text":"impl From&lt;Level1PageTableEntry&gt; for usize","synthetic":false,"types":[]},{"text":"impl From&lt;u16&gt; for Selector","synthetic":false,"types":[]},{"text":"impl From&lt;Selector&gt; for u16","synthetic":false,"types":[]},{"text":"impl From&lt;u8&gt; for SystemDescriptorType","synthetic":false,"types":[]},{"text":"impl From&lt;SystemDescriptorType&gt; for u8","synthetic":false,"types":[]},{"text":"impl From&lt;u32&gt; for GenericDescriptorFlags","synthetic":false,"types":[]},{"text":"impl From&lt;GenericDescriptorFlags&gt; for u32","synthetic":false,"types":[]},{"text":"impl From&lt;u32&gt; for SegmentDescriptorFlags","synthetic":false,"types":[]},{"text":"impl From&lt;SegmentDescriptorFlags&gt; for u32","synthetic":false,"types":[]},{"text":"impl From&lt;u32&gt; for GateDescriptorFlags","synthetic":false,"types":[]},{"text":"impl From&lt;GateDescriptorFlags&gt; for u32","synthetic":false,"types":[]},{"text":"impl From&lt;usize&gt; for FlagRegister","synthetic":false,"types":[]},{"text":"impl From&lt;FlagRegister&gt; for usize","synthetic":false,"types":[]},{"text":"impl From&lt;usize&gt; for ControlRegister0","synthetic":false,"types":[]},{"text":"impl From&lt;ControlRegister0&gt; for usize","synthetic":false,"types":[]},{"text":"impl From&lt;usize&gt; for ControlRegister4","synthetic":false,"types":[]},{"text":"impl From&lt;ControlRegister4&gt; for usize","synthetic":false,"types":[]},{"text":"impl From&lt;u64&gt; for ExtendedControlRegister0","synthetic":false,"types":[]},{"text":"impl From&lt;ExtendedControlRegister0&gt; for u64","synthetic":false,"types":[]}];
implementors["tartan_elf"] = [{"text":"impl From&lt;u8&gt; for FileClass","synthetic":false,"types":[]},{"text":"impl From&lt;FileClass&gt; for u8","synthetic":false,"types":[]},{"text":"impl From&lt;u8&gt; for Endianness","synthetic":false,"types":[]},{"text":"impl From&lt;Endianness&gt; for u8","synthetic":false,"types":[]},{"text":"impl From&lt;u8&gt; for OSABI","synthetic":false,"types":[]},{"text":"impl From&lt;OSABI&gt; for u8","synthetic":false,"types":[]},{"text":"impl From&lt;u16&gt; for FileType","synthetic":false,"types":[]},{"text":"impl From&lt;FileType&gt; for u16","synthetic":false,"types":[]},{"text":"impl From&lt;u8&gt; for Machine","synthetic":false,"types":[]},{"text":"impl From&lt;Machine&gt; for u8","synthetic":false,"types":[]},{"text":"impl From&lt;u32&gt; for ProgramSegmentType","synthetic":false,"types":[]},{"text":"impl From&lt;ProgramSegmentType&gt; for u32","synthetic":false,"types":[]},{"text":"impl From&lt;u32&gt; for ProgramSegmentFlags","synthetic":false,"types":[]},{"text":"impl From&lt;ProgramSegmentFlags&gt; for u32","synthetic":false,"types":[]},{"text":"impl From&lt;u32&gt; for SectionType","synthetic":false,"types":[]},{"text":"impl From&lt;SectionType&gt; for u32","synthetic":false,"types":[]},{"text":"impl From&lt;u32&gt; for SectionFlags","synthetic":false,"types":[]},{"text":"impl From&lt;SectionFlags&gt; for u32","synthetic":false,"types":[]},{"text":"impl From&lt;usize&gt; for SectionFlags","synthetic":false,"types":[]},{"text":"impl From&lt;SectionFlags&gt; for usize","synthetic":false,"types":[]}];
implementors["tartan_kernel"] = [{"text":"impl From&lt;usize&gt; for BlockHeader","synthetic":false,"types":[]},{"text":"impl From&lt;BlockHeader&gt; for usize","synthetic":false,"types":[]}];
implementors["tartan_pci"] = [{"text":"impl From&lt;u32&gt; for HeaderRegister0","synthetic":false,"types":[]},{"text":"impl From&lt;HeaderRegister0&gt; for u32","synthetic":false,"types":[]},{"text":"impl From&lt;u32&gt; for HeaderRegister1","synthetic":false,"types":[]},{"text":"impl From&lt;HeaderRegister1&gt; for u32","synthetic":false,"types":[]},{"text":"impl From&lt;u32&gt; for HeaderRegister2","synthetic":false,"types":[]},{"text":"impl From&lt;HeaderRegister2&gt; for u32","synthetic":false,"types":[]},{"text":"impl From&lt;u32&gt; for HeaderRegister3","synthetic":false,"types":[]},{"text":"impl From&lt;HeaderRegister3&gt; for u32","synthetic":false,"types":[]},{"text":"impl From&lt;u32&gt; for Type0HeaderRegister11","synthetic":false,"types":[]},{"text":"impl From&lt;Type0HeaderRegister11&gt; for u32","synthetic":false,"types":[]},{"text":"impl From&lt;u32&gt; for Type0HeaderRegister13","synthetic":false,"types":[]},{"text":"impl From&lt;Type0HeaderRegister13&gt; for u32","synthetic":false,"types":[]},{"text":"impl From&lt;u32&gt; for Type0HeaderRegister15","synthetic":false,"types":[]},{"text":"impl From&lt;Type0HeaderRegister15&gt; for u32","synthetic":false,"types":[]},{"text":"impl From&lt;u16&gt; for CommandRegister","synthetic":false,"types":[]},{"text":"impl From&lt;CommandRegister&gt; for u16","synthetic":false,"types":[]},{"text":"impl From&lt;u16&gt; for StatusRegister","synthetic":false,"types":[]},{"text":"impl From&lt;StatusRegister&gt; for u16","synthetic":false,"types":[]},{"text":"impl From&lt;u8&gt; for SelfTest","synthetic":false,"types":[]},{"text":"impl From&lt;SelfTest&gt; for u8","synthetic":false,"types":[]},{"text":"impl From&lt;u32&gt; for GenericCapabilityRegister","synthetic":false,"types":[]},{"text":"impl From&lt;GenericCapabilityRegister&gt; for u32","synthetic":false,"types":[]}];
implementors["tartan_uefi"] = [{"text":"impl From&lt;u32&gt; for Revision","synthetic":false,"types":[]},{"text":"impl From&lt;Revision&gt; for u32","synthetic":false,"types":[]},{"text":"impl From&lt;usize&gt; for Status","synthetic":false,"types":[]},{"text":"impl From&lt;Status&gt; for usize","synthetic":false,"types":[]},{"text":"impl From&lt;Status&gt; for Result","synthetic":false,"types":[]},{"text":"impl From&lt;u128&gt; for GUID","synthetic":false,"types":[]},{"text":"impl From&lt;u32&gt; for MemoryType","synthetic":false,"types":[]},{"text":"impl From&lt;MemoryType&gt; for u32","synthetic":false,"types":[]},{"text":"impl From&lt;u32&gt; for AllocateType","synthetic":false,"types":[]},{"text":"impl From&lt;AllocateType&gt; for u32","synthetic":false,"types":[]},{"text":"impl From&lt;u64&gt; for MemoryAttributes","synthetic":false,"types":[]},{"text":"impl From&lt;MemoryAttributes&gt; for u64","synthetic":false,"types":[]},{"text":"impl From&lt;u32&gt; for OpenProtocolAttributes","synthetic":false,"types":[]},{"text":"impl From&lt;OpenProtocolAttributes&gt; for u32","synthetic":false,"types":[]},{"text":"impl From&lt;u64&gt; for FileMode","synthetic":false,"types":[]},{"text":"impl From&lt;FileMode&gt; for u64","synthetic":false,"types":[]},{"text":"impl From&lt;u64&gt; for FileAttributes","synthetic":false,"types":[]},{"text":"impl From&lt;FileAttributes&gt; for u64","synthetic":false,"types":[]}];
if (window.register_implementors) {window.register_implementors(implementors);} else {window.pending_implementors = implementors;}})()