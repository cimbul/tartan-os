(function() {var implementors = {};
implementors["aml_parse"] = [{"text":"impl UnwindSafe for UsageError","synthetic":true,"types":[]}];
implementors["tartan_acpi"] = [{"text":"impl UnwindSafe for MethodSignature","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; UnwindSafe for ParserState&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl UnwindSafe for NameSeg","synthetic":true,"types":[]},{"text":"impl UnwindSafe for NameString","synthetic":true,"types":[]},{"text":"impl UnwindSafe for PathAnchor","synthetic":true,"types":[]},{"text":"impl UnwindSafe for SimpleName","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; UnwindSafe for SuperName&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; UnwindSafe for ComputationalData&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; UnwindSafe for Buffer&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; UnwindSafe for Package&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; UnwindSafe for VarPackage&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; UnwindSafe for PackageElement&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; UnwindSafe for DataObject&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; UnwindSafe for DataRefObject&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; UnwindSafe for TermObject&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; UnwindSafe for TermArg&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; UnwindSafe for NameSpaceModifier&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; UnwindSafe for NamedObject&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl UnwindSafe for FieldFlags","synthetic":true,"types":[]},{"text":"impl UnwindSafe for AccessType","synthetic":true,"types":[]},{"text":"impl UnwindSafe for UpdateRule","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; UnwindSafe for FieldElement&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl UnwindSafe for AccessAttrib","synthetic":true,"types":[]},{"text":"impl UnwindSafe for MethodFlags","synthetic":true,"types":[]},{"text":"impl UnwindSafe for RegionSpace","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; UnwindSafe for StatementOpcode&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; UnwindSafe for ReferenceExpressionOpcode&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; UnwindSafe for ExpressionOpcode&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl UnwindSafe for MatchOpcode","synthetic":true,"types":[]},{"text":"impl UnwindSafe for ObjectType","synthetic":true,"types":[]},{"text":"impl UnwindSafe for ArgObject","synthetic":true,"types":[]},{"text":"impl UnwindSafe for LocalObject","synthetic":true,"types":[]},{"text":"impl UnwindSafe for DebugObject","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; UnwindSafe for AMLTable&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl UnwindSafe for RootDescriptionPointerV1","synthetic":true,"types":[]},{"text":"impl UnwindSafe for RootDescriptionPointerV2","synthetic":true,"types":[]},{"text":"impl UnwindSafe for DescriptionHeader","synthetic":true,"types":[]},{"text":"impl UnwindSafe for FixedDescription","synthetic":true,"types":[]},{"text":"impl UnwindSafe for GenericAddress","synthetic":true,"types":[]},{"text":"impl UnwindSafe for AddressSpace","synthetic":true,"types":[]},{"text":"impl UnwindSafe for AccessSize","synthetic":true,"types":[]},{"text":"impl UnwindSafe for FixedFlags","synthetic":true,"types":[]}];
implementors["tartan_devicetree"] = [{"text":"impl&lt;'a&gt; UnwindSafe for Tree&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; UnwindSafe for StructureData&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; UnwindSafe for Value&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl UnwindSafe for MemoryReservation","synthetic":true,"types":[]}];
implementors["tartan_parsers"] = [{"text":"impl&lt;'a, I&gt; UnwindSafe for Position&lt;'a, I&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;I: RefUnwindSafe,&nbsp;</span>","synthetic":true,"types":[]},{"text":"impl&lt;'a, I&gt; UnwindSafe for ErrorWithPosition&lt;'a, I&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;I: UnwindSafe,&nbsp;</span>","synthetic":true,"types":[]}];
implementors["tartan_uefi"] = [{"text":"impl UnwindSafe for BootAllocator","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; UnwindSafe for OutputStream&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; UnwindSafe for Logger&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl UnwindSafe for SimpleTextInput","synthetic":true,"types":[]},{"text":"impl UnwindSafe for SimpleTextOutput","synthetic":true,"types":[]},{"text":"impl UnwindSafe for SimpleTextOutputMode","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; UnwindSafe for LoadedImage&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl UnwindSafe for SimpleFileSystem","synthetic":true,"types":[]},{"text":"impl UnwindSafe for File","synthetic":true,"types":[]},{"text":"impl UnwindSafe for FileMode","synthetic":true,"types":[]},{"text":"impl UnwindSafe for FileAttributes","synthetic":true,"types":[]},{"text":"impl UnwindSafe for Handle","synthetic":true,"types":[]},{"text":"impl UnwindSafe for Revision","synthetic":true,"types":[]},{"text":"impl UnwindSafe for Status","synthetic":true,"types":[]},{"text":"impl UnwindSafe for TableHeader","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; UnwindSafe for SystemTable&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl UnwindSafe for RuntimeServices","synthetic":true,"types":[]},{"text":"impl UnwindSafe for BootServices","synthetic":true,"types":[]},{"text":"impl UnwindSafe for GUID","synthetic":true,"types":[]},{"text":"impl UnwindSafe for ConfigurationTable","synthetic":true,"types":[]},{"text":"impl UnwindSafe for MemoryDescriptor","synthetic":true,"types":[]},{"text":"impl UnwindSafe for MemoryMap","synthetic":true,"types":[]},{"text":"impl UnwindSafe for MemoryType","synthetic":true,"types":[]},{"text":"impl UnwindSafe for AllocateType","synthetic":true,"types":[]},{"text":"impl UnwindSafe for MemoryAttributes","synthetic":true,"types":[]},{"text":"impl UnwindSafe for OpenProtocolAttributes","synthetic":true,"types":[]}];
if (window.register_implementors) {window.register_implementors(implementors);} else {window.pending_implementors = implementors;}})()