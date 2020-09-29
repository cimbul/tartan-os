(function() {var implementors = {};
implementors["tartan_acpi"] = [{"text":"impl Clone for MethodSignature","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; Clone for ParserState&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl Clone for NameSeg","synthetic":false,"types":[]},{"text":"impl Clone for NameString","synthetic":false,"types":[]},{"text":"impl Clone for PathAnchor","synthetic":false,"types":[]},{"text":"impl Clone for SimpleName","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; Clone for SuperName&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; Clone for ComputationalData&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; Clone for Buffer&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; Clone for Package&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; Clone for VarPackage&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; Clone for PackageElement&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; Clone for DataObject&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; Clone for DataRefObject&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; Clone for TermObject&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; Clone for TermArg&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; Clone for NameSpaceModifier&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; Clone for NamedObject&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl Clone for FieldFlags","synthetic":false,"types":[]},{"text":"impl Clone for AccessType","synthetic":false,"types":[]},{"text":"impl Clone for UpdateRule","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; Clone for FieldElement&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl Clone for AccessAttrib","synthetic":false,"types":[]},{"text":"impl Clone for MethodFlags","synthetic":false,"types":[]},{"text":"impl Clone for RegionSpace","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; Clone for StatementOpcode&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; Clone for ReferenceExpressionOpcode&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; Clone for ExpressionOpcode&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl Clone for MatchOpcode","synthetic":false,"types":[]},{"text":"impl Clone for ObjectType","synthetic":false,"types":[]},{"text":"impl Clone for ArgObject","synthetic":false,"types":[]},{"text":"impl Clone for LocalObject","synthetic":false,"types":[]},{"text":"impl Clone for DebugObject","synthetic":false,"types":[]},{"text":"impl Clone for RootDescriptionPointerV1","synthetic":false,"types":[]},{"text":"impl Clone for RootDescriptionPointerV2","synthetic":false,"types":[]},{"text":"impl Clone for DescriptionHeader","synthetic":false,"types":[]},{"text":"impl Clone for FixedDescription","synthetic":false,"types":[]},{"text":"impl Clone for GenericAddress","synthetic":false,"types":[]},{"text":"impl Clone for AddressSpace","synthetic":false,"types":[]},{"text":"impl Clone for AccessSize","synthetic":false,"types":[]},{"text":"impl Clone for FixedFlags","synthetic":false,"types":[]}];
implementors["tartan_arch"] = [{"text":"impl Clone for ControlRegister8","synthetic":false,"types":[]},{"text":"impl Clone for ExtendedFeatureEnableRegister","synthetic":false,"types":[]},{"text":"impl Clone for Features","synthetic":false,"types":[]},{"text":"impl Clone for ExtendedFeatures","synthetic":false,"types":[]},{"text":"impl Clone for AddressSpaceSizes","synthetic":false,"types":[]},{"text":"impl Clone for ControlRegister2","synthetic":false,"types":[]},{"text":"impl Clone for ControlRegister3","synthetic":false,"types":[]},{"text":"impl Clone for Level2PageTableEntry","synthetic":false,"types":[]},{"text":"impl Clone for Level1PageTableEntry","synthetic":false,"types":[]},{"text":"impl Clone for FlagRegister","synthetic":false,"types":[]},{"text":"impl Clone for ControlRegister0","synthetic":false,"types":[]},{"text":"impl Clone for ControlRegister4","synthetic":false,"types":[]},{"text":"impl Clone for ExtendedControlRegister0","synthetic":false,"types":[]}];
implementors["tartan_devicetree"] = [{"text":"impl&lt;'a&gt; Clone for Tree&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl Clone for MemoryReservation","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; Clone for StructureData&lt;'a&gt;","synthetic":false,"types":[]}];
implementors["tartan_parsers"] = [{"text":"impl&lt;'a, I:&nbsp;Clone&gt; Clone for ErrorWithPosition&lt;'a, I&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;I: From&lt;&amp;'a [u8]&gt; + AsBytes,&nbsp;</span>","synthetic":false,"types":[]}];
implementors["tartan_pci"] = [{"text":"impl Clone for ConfigSelector","synthetic":false,"types":[]},{"text":"impl Clone for MemMapConfigAccess","synthetic":false,"types":[]},{"text":"impl Clone for IOConfigAccess","synthetic":false,"types":[]},{"text":"impl Clone for HeaderRegister0","synthetic":false,"types":[]},{"text":"impl Clone for HeaderRegister1","synthetic":false,"types":[]},{"text":"impl Clone for HeaderRegister2","synthetic":false,"types":[]},{"text":"impl Clone for HeaderRegister3","synthetic":false,"types":[]},{"text":"impl Clone for Type0HeaderRegister11","synthetic":false,"types":[]},{"text":"impl Clone for Type0HeaderRegister13","synthetic":false,"types":[]},{"text":"impl Clone for Type0HeaderRegister15","synthetic":false,"types":[]},{"text":"impl Clone for CommandRegister","synthetic":false,"types":[]},{"text":"impl Clone for StatusRegister","synthetic":false,"types":[]},{"text":"impl Clone for SelfTest","synthetic":false,"types":[]},{"text":"impl Clone for BaseAddressRegister","synthetic":false,"types":[]},{"text":"impl Clone for AddressSpace","synthetic":false,"types":[]},{"text":"impl Clone for AddressWidth","synthetic":false,"types":[]},{"text":"impl Clone for GenericCapabilityRegister","synthetic":false,"types":[]}];
implementors["tartan_uefi"] = [{"text":"impl&lt;'a&gt; Clone for OutputStream&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl Clone for Handle","synthetic":false,"types":[]},{"text":"impl Clone for Revision","synthetic":false,"types":[]},{"text":"impl Clone for Status","synthetic":false,"types":[]},{"text":"impl Clone for GUID","synthetic":false,"types":[]},{"text":"impl Clone for MemoryType","synthetic":false,"types":[]},{"text":"impl Clone for AllocateType","synthetic":false,"types":[]},{"text":"impl Clone for MemoryAttributes","synthetic":false,"types":[]},{"text":"impl Clone for OpenProtocolAttributes","synthetic":false,"types":[]},{"text":"impl Clone for SimpleTextOutputMode","synthetic":false,"types":[]}];
if (window.register_implementors) {window.register_implementors(implementors);} else {window.pending_implementors = implementors;}})()