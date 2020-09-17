(function() {var implementors = {};
implementors["aml_parse"] = [{"text":"impl Freeze for UsageError","synthetic":true,"types":[]}];
implementors["tartan_acpi"] = [{"text":"impl Freeze for RootDescriptionPointerV1","synthetic":true,"types":[]},{"text":"impl Freeze for RootDescriptionPointerV2","synthetic":true,"types":[]},{"text":"impl Freeze for DescriptionHeader","synthetic":true,"types":[]},{"text":"impl Freeze for FixedDescription","synthetic":true,"types":[]},{"text":"impl Freeze for GenericAddress","synthetic":true,"types":[]},{"text":"impl Freeze for AddressSpace","synthetic":true,"types":[]},{"text":"impl Freeze for AccessSize","synthetic":true,"types":[]},{"text":"impl Freeze for FixedFlags","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Freeze for AMLTable&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Freeze for MethodSignature","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Freeze for ParserState&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Freeze for NameSeg","synthetic":true,"types":[]},{"text":"impl Freeze for NameString","synthetic":true,"types":[]},{"text":"impl Freeze for PathAnchor","synthetic":true,"types":[]},{"text":"impl Freeze for SimpleName","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Freeze for SuperName&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Freeze for Buffer&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Freeze for Package&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Freeze for VarPackage&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Freeze for ComputationalData&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Freeze for PackageElement&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Freeze for DataObject&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Freeze for DataRefObject&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Freeze for FieldFlags","synthetic":true,"types":[]},{"text":"impl Freeze for MethodFlags","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Freeze for TermObject&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Freeze for TermArg&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Freeze for NameSpaceModifier&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Freeze for NamedObject&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Freeze for AccessType","synthetic":true,"types":[]},{"text":"impl Freeze for UpdateRule","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Freeze for FieldElement&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Freeze for AccessAttrib","synthetic":true,"types":[]},{"text":"impl Freeze for RegionSpace","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Freeze for StatementOpcode&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Freeze for ReferenceExpressionOpcode&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Freeze for ExpressionOpcode&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Freeze for MatchOpcode","synthetic":true,"types":[]},{"text":"impl Freeze for ObjectType","synthetic":true,"types":[]},{"text":"impl Freeze for DebugObject","synthetic":true,"types":[]},{"text":"impl Freeze for ArgObject","synthetic":true,"types":[]},{"text":"impl Freeze for LocalObject","synthetic":true,"types":[]}];
implementors["tartan_devicetree"] = [{"text":"impl&lt;'a&gt; Freeze for Tree&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Freeze for MemoryReservation","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Freeze for StructureData&lt;'a&gt;","synthetic":true,"types":[]}];
implementors["tartan_parsers"] = [{"text":"impl&lt;'a, I&gt; Freeze for Position&lt;'a, I&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a, I&gt; Freeze for ErrorWithPosition&lt;'a, I&gt;","synthetic":true,"types":[]}];
implementors["tartan_pci"] = [{"text":"impl Freeze for ConfigSelector","synthetic":true,"types":[]},{"text":"impl Freeze for MemMapConfigAccess","synthetic":true,"types":[]},{"text":"impl Freeze for HeaderRegister0","synthetic":true,"types":[]},{"text":"impl Freeze for HeaderRegister1","synthetic":true,"types":[]},{"text":"impl Freeze for HeaderRegister2","synthetic":true,"types":[]},{"text":"impl Freeze for HeaderRegister3","synthetic":true,"types":[]},{"text":"impl Freeze for Type0HeaderRegister11","synthetic":true,"types":[]},{"text":"impl Freeze for Type0HeaderRegister13","synthetic":true,"types":[]},{"text":"impl Freeze for Type0HeaderRegister15","synthetic":true,"types":[]},{"text":"impl Freeze for CommandRegister","synthetic":true,"types":[]},{"text":"impl Freeze for StatusRegister","synthetic":true,"types":[]},{"text":"impl Freeze for SelfTest","synthetic":true,"types":[]},{"text":"impl Freeze for BaseAddressRegister","synthetic":true,"types":[]},{"text":"impl Freeze for AddressSpace","synthetic":true,"types":[]},{"text":"impl Freeze for AddressWidth","synthetic":true,"types":[]}];
implementors["tartan_uefi"] = [{"text":"impl Freeze for Handle","synthetic":true,"types":[]},{"text":"impl Freeze for Revision","synthetic":true,"types":[]},{"text":"impl Freeze for Status","synthetic":true,"types":[]},{"text":"impl Freeze for TableHeader","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Freeze for SystemTable&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Freeze for RuntimeServices","synthetic":true,"types":[]},{"text":"impl Freeze for BootServices","synthetic":true,"types":[]},{"text":"impl Freeze for GUID","synthetic":true,"types":[]},{"text":"impl Freeze for ConfigurationTable","synthetic":true,"types":[]},{"text":"impl Freeze for MemoryDescriptor","synthetic":true,"types":[]},{"text":"impl Freeze for MemoryMap","synthetic":true,"types":[]},{"text":"impl Freeze for MemoryType","synthetic":true,"types":[]},{"text":"impl Freeze for AllocateType","synthetic":true,"types":[]},{"text":"impl Freeze for MemoryAttributes","synthetic":true,"types":[]},{"text":"impl Freeze for OpenProtocolAttributes","synthetic":true,"types":[]},{"text":"impl Freeze for BootAllocator","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Freeze for OutputStream&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Freeze for Logger&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Freeze for SimpleTextInput","synthetic":true,"types":[]},{"text":"impl Freeze for SimpleTextOutput","synthetic":true,"types":[]},{"text":"impl Freeze for SimpleTextOutputMode","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Freeze for LoadedImage&lt;'a&gt;","synthetic":true,"types":[]}];
if (window.register_implementors) {window.register_implementors(implementors);} else {window.pending_implementors = implementors;}})()