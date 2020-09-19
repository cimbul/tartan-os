(function() {var implementors = {};
implementors["aml_parse"] = [{"text":"impl Debug for UsageError","synthetic":false,"types":[]}];
implementors["tartan_acpi"] = [{"text":"impl Debug for MethodSignature","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; Debug for ParserState&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; Debug for AMLTable&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl Debug for NameSeg","synthetic":false,"types":[]},{"text":"impl Debug for NameString","synthetic":false,"types":[]},{"text":"impl Debug for PathAnchor","synthetic":false,"types":[]},{"text":"impl Debug for SimpleName","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; Debug for SuperName&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; Debug for ComputationalData&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; Debug for Buffer&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; Debug for Package&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; Debug for VarPackage&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; Debug for PackageElement&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; Debug for DataObject&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; Debug for DataRefObject&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; Debug for TermObject&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; Debug for TermArg&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; Debug for NameSpaceModifier&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; Debug for NamedObject&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl Debug for FieldFlags","synthetic":false,"types":[]},{"text":"impl Debug for AccessType","synthetic":false,"types":[]},{"text":"impl Debug for UpdateRule","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; Debug for FieldElement&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl Debug for AccessAttrib","synthetic":false,"types":[]},{"text":"impl Debug for MethodFlags","synthetic":false,"types":[]},{"text":"impl Debug for RegionSpace","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; Debug for StatementOpcode&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; Debug for ReferenceExpressionOpcode&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; Debug for ExpressionOpcode&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl Debug for MatchOpcode","synthetic":false,"types":[]},{"text":"impl Debug for ObjectType","synthetic":false,"types":[]},{"text":"impl Debug for ArgObject","synthetic":false,"types":[]},{"text":"impl Debug for LocalObject","synthetic":false,"types":[]},{"text":"impl Debug for DebugObject","synthetic":false,"types":[]},{"text":"impl Debug for RootDescriptionPointerV1","synthetic":false,"types":[]},{"text":"impl Debug for RootDescriptionPointerV2","synthetic":false,"types":[]},{"text":"impl Debug for DescriptionHeader","synthetic":false,"types":[]},{"text":"impl Debug for FixedDescription","synthetic":false,"types":[]},{"text":"impl Debug for GenericAddress","synthetic":false,"types":[]},{"text":"impl Debug for AddressSpace","synthetic":false,"types":[]},{"text":"impl Debug for AccessSize","synthetic":false,"types":[]},{"text":"impl Debug for FixedFlags","synthetic":false,"types":[]}];
implementors["tartan_devicetree"] = [{"text":"impl&lt;'a&gt; Debug for Tree&lt;'a&gt;","synthetic":false,"types":[]},{"text":"impl Debug for MemoryReservation","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; Debug for StructureData&lt;'a&gt;","synthetic":false,"types":[]}];
implementors["tartan_parsers"] = [{"text":"impl&lt;'a, I:&nbsp;Debug&gt; Debug for ErrorWithPosition&lt;'a, I&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;I: From&lt;&amp;'a [u8]&gt; + AsBytes,&nbsp;</span>","synthetic":false,"types":[]}];
implementors["tartan_pci"] = [{"text":"impl Debug for ConfigSelector","synthetic":false,"types":[]},{"text":"impl Debug for MemMapConfigAccess","synthetic":false,"types":[]},{"text":"impl Debug for IOConfigAccess","synthetic":false,"types":[]},{"text":"impl Debug for HeaderRegister0","synthetic":false,"types":[]},{"text":"impl Debug for HeaderRegister1","synthetic":false,"types":[]},{"text":"impl Debug for HeaderRegister2","synthetic":false,"types":[]},{"text":"impl Debug for HeaderRegister3","synthetic":false,"types":[]},{"text":"impl Debug for Type0HeaderRegister11","synthetic":false,"types":[]},{"text":"impl Debug for Type0HeaderRegister13","synthetic":false,"types":[]},{"text":"impl Debug for Type0HeaderRegister15","synthetic":false,"types":[]},{"text":"impl Debug for CommandRegister","synthetic":false,"types":[]},{"text":"impl Debug for StatusRegister","synthetic":false,"types":[]},{"text":"impl Debug for SelfTest","synthetic":false,"types":[]},{"text":"impl Debug for BaseAddressRegister","synthetic":false,"types":[]},{"text":"impl Debug for AddressSpace","synthetic":false,"types":[]},{"text":"impl Debug for AddressWidth","synthetic":false,"types":[]},{"text":"impl Debug for GenericCapabilityRegister","synthetic":false,"types":[]}];
implementors["tartan_uefi"] = [{"text":"impl Debug for Handle","synthetic":false,"types":[]},{"text":"impl Debug for Revision","synthetic":false,"types":[]},{"text":"impl Debug for Status","synthetic":false,"types":[]},{"text":"impl Debug for GUID","synthetic":false,"types":[]},{"text":"impl Debug for MemoryType","synthetic":false,"types":[]},{"text":"impl Debug for AllocateType","synthetic":false,"types":[]},{"text":"impl Debug for MemoryAttributes","synthetic":false,"types":[]},{"text":"impl Debug for OpenProtocolAttributes","synthetic":false,"types":[]},{"text":"impl Debug for SimpleTextInput","synthetic":false,"types":[]},{"text":"impl Debug for SimpleTextOutputMode","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; Debug for LoadedImage&lt;'a&gt;","synthetic":false,"types":[]}];
if (window.register_implementors) {window.register_implementors(implementors);} else {window.pending_implementors = implementors;}})()