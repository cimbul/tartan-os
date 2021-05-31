//! Abstract syntax tree for ACPI Machine Language

#![allow(clippy::module_name_repetitions)]


use crate::DescriptionHeader;
use alloc::vec::Vec;


pub mod parse;
pub mod interpreter;


/// An ACPI table that contains AML, such as the DSDT or an SSDT.
#[derive(Debug, PartialEq, Eq)]
pub struct AMLTable<'a> {
    /// Common ACPI table header
    pub header: DescriptionHeader,
    /// Top-level AML objects defined in the table
    pub body: Vec<term::TermObject<'a>>,
}


macro_rules! from_impl {
    [$( <$a:lifetime> )? ($x:ident: $from:ty) -> $to:ty = $imp:expr] => {
        impl$(<$a>)? From<$from> for $to {
            fn from($x: $from) -> $to { $imp }
        }
    };
}


/// Names of objects, arguments, and references
pub mod name {
    use super::misc::{ArgObject, LocalObject};
    use super::term::ReferenceExpressionOpcode;
    use alloc::boxed::Box;
    use alloc::string::ToString;
    use alloc::vec;
    use alloc::vec::Vec;
    use core::fmt;


    /// Convert a list of segments in various formats into a path vector
    ///
    /// ```
    /// # use tartan_acpi::aml::name::{NameSeg, to_path};
    /// #
    /// assert_eq!(to_path(&[b"ABCD", b"EFGH"]), vec![NameSeg(*b"ABCD"), NameSeg(*b"EFGH")]);
    /// ```
    pub fn to_path<T: Copy + Into<NameSeg>>(path: &[T]) -> Vec<NameSeg> {
        path.iter().map(|s| (*s).into()).collect()
    }


    /// Four-character name segment, allowing underscores, uppercase letters, and digits
    /// (except at the beginning).
    ///
    /// The ASL compiler uses underscores to pad the end of names shorter than 4 chars.
    #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
    pub struct NameSeg(pub [u8; 4]);

    impl fmt::Display for NameSeg {
        fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
            let str_name = core::str::from_utf8(&self.0).unwrap_or("<invalid>");
            f.write_str(str_name)
        }
    }

    impl fmt::Debug for NameSeg {
        fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
            write!(f, "NameSeg[{:?}]", self.to_string())
        }
    }

    from_impl!((n: &[u8; 4]) -> NameSeg = NameSeg(*n));


    /// Fully qualified object path, either absolute or relative.
    #[derive(Clone, PartialEq, Eq)]
    pub struct NameString {
        /// Specifies how to resolve this path as absolute or relative.
        pub anchor: PathAnchor,
        /// Segments of the path
        pub path: Vec<NameSeg>,
    }

    impl NameString {
        pub fn empty() -> Self {
            NameString::new::<NameSeg>(&[])
        }

        pub fn new<T: Copy + Into<NameSeg>>(path: &[T]) -> Self {
            NameString::new_parent(0, path)
        }

        pub fn new_root<T: Copy + Into<NameSeg>>(path: &[T]) -> Self {
            NameString { path: to_path(path), anchor: PathAnchor::Root }
        }

        pub fn new_parent<T: Copy + Into<NameSeg>>(n: usize, path: &[T]) -> Self {
            NameString { path: to_path(path), anchor: PathAnchor::Parent(n) }
        }

        /// Convert to an absolute namespace path in the context of the given scope,
        /// treating the name as new declaration/definition.
        ///
        /// If this name is a reference that must be looked up in the namespace, then use
        /// [`NameString::resolve_as_ref`] instead.
        ///
        /// ```
        /// # use tartan_acpi::aml::name::{NameString, to_path};
        /// #
        /// let scope = to_path(&[b"A___", b"B___"]);
        ///
        /// let child = NameString::new(&[b"X___"]);
        /// let up_1  = NameString::new_parent(1, &[b"X___"]);
        /// let up_2  = NameString::new_parent(2, &[b"X___"]);
        /// let up_3  = NameString::new_parent(3, &[b"X___"]);
        /// let abs   = NameString::new_root(&[b"Y___", b"Z___"]);
        ///
        /// assert_eq!(child.resolve_as_decl(&scope), Some(to_path(&[b"A___", b"B___", b"X___"])));
        /// assert_eq!(up_1.resolve_as_decl(&scope),  Some(to_path(&[b"A___", b"X___"])));
        /// assert_eq!(up_2.resolve_as_decl(&scope),  Some(to_path(&[b"X___"])));
        /// assert_eq!(up_3.resolve_as_decl(&scope),  None);
        /// assert_eq!(abs.resolve_as_decl(&scope),   Some(to_path(&[b"Y___", b"Z___"])));
        /// ```
        ///
        /// # Errors
        ///
        /// Returns `None` if this `NameString` is anchored to a parent scope and the
        /// result would be outside the namespace root.
        pub fn resolve_as_decl(&self, scope: &[NameSeg]) -> Option<Vec<NameSeg>> {
            self.resolve_as_ref(scope).into_iter().next()
        }

        /// List all possible absolute pathnames that this name may refer to in the
        /// context of the given scope.
        ///
        /// Use this method if the name is a reference that must be looked up in the
        /// namespace. For names that define new objects, see
        /// [`NameString::resolve_as_decl`].
        ///
        /// As defined by §5.3 of ACPI 6.3, there are distinct rules for two cases:
        ///   * Unanchored one-segment names are looked up in the current scope, then the
        ///     parent scope, etc. until a match is found.
        ///   * All other names are resolved relative to the current scope *only*.
        ///
        /// ```
        /// # use tartan_acpi::aml::name::{NameSeg, NameString, to_path};
        /// #
        /// let scope = to_path(&[b"A___", b"B___"]);
        ///
        /// let simple = NameString::new(&[b"X___"]);
        /// let multi  = NameString::new(&[b"X___", b"Y___"]);
        /// let up_1   = NameString::new_parent(1, &[b"X___"]);
        /// let up_2   = NameString::new_parent(2, &[b"X___"]);
        /// let up_3   = NameString::new_parent(3, &[b"X___"]);
        ///
        /// // Look up unanchored one-segment names in parent scopes
        /// assert_eq!(
        ///     simple.resolve_as_ref(&scope),
        ///     vec![
        ///         to_path(&[b"A___", b"B___", b"X___"]),
        ///         to_path(&[b"A___", b"X___"]),
        ///         to_path(&[b"X___"]),
        ///     ],
        /// );
        ///
        /// // Others only resolve against current scope
        /// assert_eq!(multi.resolve_as_ref(&scope), vec![to_path(&[b"A___", b"B___", b"X___", b"Y___"])]);
        /// assert_eq!(up_1.resolve_as_ref(&scope),  vec![to_path(&[b"A___", b"X___"])]);
        /// assert_eq!(up_2.resolve_as_ref(&scope),  vec![to_path(&[b"X___"])]);
        /// assert_eq!(up_3.resolve_as_ref(&scope),  Vec::<Vec<NameSeg>>::new());
        /// ```
        ///
        /// Will return an empty vector if this `NameString` is anchored to a parent scope
        /// and the result would be outside the namespace root.
        pub fn resolve_as_ref(&self, scope: &[NameSeg]) -> Vec<Vec<NameSeg>> {
            match self.anchor {
                PathAnchor::Root => vec![self.path.clone()],
                PathAnchor::Parent(n) if n == scope.len() => vec![self.path.clone()],
                PathAnchor::Parent(0) if self.path.len() == 1 => {
                    let ends = (0..=scope.len()).rev();
                    ends.map(|end| [&scope[..end], &self.path].concat()).collect()
                }
                PathAnchor::Parent(n) if n < scope.len() => {
                    let end = scope.len() - n;
                    vec![[&scope[..end], &self.path].concat()]
                }
                // n > scope.len(), so this falls out of the namespace root
                PathAnchor::Parent(_) => vec![],
            }
        }
    }

    impl fmt::Display for NameString {
        /// Format a `NameString` like a path in ASL
        ///
        /// ```
        /// # use tartan_acpi::aml::name::NameString;
        /// #
        /// let a = NameString::empty();
        /// assert_eq!(format!("{}", a), "");
        ///
        /// let b = NameString::new(&[b"ASDF", b"_123"]);
        /// assert_eq!(format!("{}", b), "ASDF._123");
        ///
        /// let c = NameString::new_parent(2, &[b"FOO_", b"BAR_"]);
        /// assert_eq!(format!("{}", c), "^^FOO_.BAR_");
        ///
        /// let d = NameString::new_root(&[b"X___"]);
        /// assert_eq!(format!("{}", d), "\\X___");
        /// ```
        fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
            write!(f, "{}", self.anchor)?;
            if !self.path.is_empty() {
                write!(f, "{}", self.path[0])?;
                for n in self.path.iter().skip(1) {
                    write!(f, ".{}", n)?;
                }
            }
            Ok(())
        }
    }

    impl fmt::Debug for NameString {
        fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
            write!(f, "NameString[{:?}]", self.to_string())
        }
    }

    from_impl!((n: &[u8; 4]) -> NameString = NameString::new(&[n]));
    from_impl!((n: NameSeg) -> NameString = NameString::new(&[n]));


    /// Indicates whether a name is absolute or relative to the current or parent scope.
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum PathAnchor {
        Root,
        Parent(usize),
    }

    impl fmt::Display for PathAnchor {
        fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
            match self {
                PathAnchor::Root => f.write_str("\\"),
                PathAnchor::Parent(n) => {
                    for _ in 0..(*n) {
                        f.write_str("^")?;
                    }
                    Ok(())
                }
            }
        }
    }


    /// A named object or variable.
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum SimpleName {
        Name(NameString),
        Arg(ArgObject),
        Local(LocalObject),
    }

    from_impl!((n: NameString) -> SimpleName = SimpleName::Name(n));
    from_impl!((a: ArgObject) -> SimpleName = SimpleName::Arg(a));
    from_impl!((l: LocalObject) -> SimpleName = SimpleName::Local(l));


    /// A named object, variable, reference expression, or debug object.
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum SuperName<'a> {
        Name(SimpleName),
        Debug,
        Reference(Box<ReferenceExpressionOpcode<'a>>),
    }

    from_impl!(<'a>(n: SimpleName) -> SuperName<'a> = SuperName::Name(n));
    from_impl!(<'a>(n: NameString) -> SuperName<'a> = SimpleName::from(n).into());
    from_impl!(<'a>(a: ArgObject) -> SuperName<'a> = SimpleName::from(a).into());
    from_impl!(<'a>(l: LocalObject) -> SuperName<'a> = SimpleName::from(l).into());
    from_impl!(
        <'a>(r: ReferenceExpressionOpcode<'a>) -> SuperName<'a> =
            SuperName::Reference(Box::new(r)));


    /// Location to store the result of an operation
    pub type Target<'a> = Option<SuperName<'a>>;
}



/// Data resources
pub mod data {
    use super::name::NameString;
    use super::term::TermArg;
    use alloc::vec::Vec;

    /// Single value resolved at compile time.
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum ComputationalData<'a> {
        Byte(u8),
        Word(u16),
        DWord(u32),
        QWord(u64),
        String(&'a str),
        Zero,
        One,
        Ones,
        Revision,
        Buffer(Buffer<'a>),
    }

    /// Block of raw bytes, optionally initialized in whole or in part.
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct Buffer<'a> {
        pub size: TermArg<'a>,
        pub initializer: &'a [u8],
    }

    /// Group of related data elements, optionally initialized in whole or in part.
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct Package<'a> {
        pub count: u8,
        pub initializers: Vec<PackageElement<'a>>,
    }

    /// Group of related data elements, with a number of elements only known at runtime.
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct VarPackage<'a> {
        pub count: TermArg<'a>,
        pub initializers: Vec<PackageElement<'a>>,
    }


    /// A name or reference that constitutes part of a [`Package`] (or [`VarPackage`]).
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum PackageElement<'a> {
        Ref(DataRefObject<'a>),
        Name(NameString),
    }

    from_impl!(<'a>(r: DataRefObject<'a>) -> PackageElement<'a> = PackageElement::Ref(r));
    from_impl!(<'a>(n: NameString) -> PackageElement<'a> = PackageElement::Name(n));


    /// Data resolved at compile time, possibly grouped in a package.
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum DataObject<'a> {
        Data(ComputationalData<'a>),
        Package(Package<'a>),
        VarPackage(VarPackage<'a>),
    }

    from_impl!(<'a>(d: ComputationalData<'a>) -> DataObject<'a> = DataObject::Data(d));
    from_impl!(<'a>(p: Package<'a>) -> DataObject<'a> = DataObject::Package(p));
    from_impl!(<'a>(p: VarPackage<'a>) -> DataObject<'a> = DataObject::VarPackage(p));


    // TODO: Description
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum DataRefObject<'a> {
        Data(DataObject<'a>),
        ObjectReference(u64),
        DefinitionBlockHandle(u64),
    }

    from_impl!(<'a>(d: DataObject<'a>) -> DataRefObject<'a> = DataRefObject::Data(d));
    from_impl!(<'a>(d: ComputationalData<'a>) -> DataRefObject<'a> = DataObject::from(d).into());
    from_impl!(<'a>(p: Package<'a>) -> DataRefObject<'a> = DataObject::from(p).into());
    from_impl!(<'a>(p: VarPackage<'a>) -> DataRefObject<'a> = DataObject::from(p).into());
}



/// Top-level terms and opcodes.
pub mod term {
    use super::data::{
        Buffer, ComputationalData, DataObject, DataRefObject, Package, VarPackage,
    };
    use super::misc::{ArgObject, LocalObject};
    use super::name::{NameSeg, NameString, SimpleName, SuperName, Target};
    use alloc::boxed::Box;
    use alloc::vec::Vec;

    /// Top-level, most general term type where the value (if any) is discarded.
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum TermObject<'a> {
        Modifier(Box<NameSpaceModifier<'a>>),
        Named(Box<NamedObject<'a>>),
        Statement(Box<StatementOpcode<'a>>),
        Expression(Box<ExpressionOpcode<'a>>),
    }

    from_impl!(<'a>(m: NameSpaceModifier<'a>) -> TermObject<'a> = TermObject::Modifier(Box::new(m)));
    from_impl!(<'a>(n: NamedObject<'a>) -> TermObject<'a> = TermObject::Named(Box::new(n)));
    from_impl!(<'a>(s: StatementOpcode<'a>) -> TermObject<'a> = TermObject::Statement(Box::new(s)));
    from_impl!(<'a>(e: ExpressionOpcode<'a>) -> TermObject<'a> = TermObject::Expression(Box::new(e)));
    from_impl!(<'a>(r: ReferenceExpressionOpcode<'a>) -> TermObject<'a> = ExpressionOpcode::from(r).into());
    from_impl!(<'a>(b: Buffer<'a>) -> TermObject<'a> = ExpressionOpcode::from(b).into());
    from_impl!(<'a>(p: Package<'a>) -> TermObject<'a> = ExpressionOpcode::from(p).into());
    from_impl!(<'a>(p: VarPackage<'a>) -> TermObject<'a> = ExpressionOpcode::from(p).into());


    /// Term that resolves to a value.
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum TermArg<'a> {
        Expression(Box<ExpressionOpcode<'a>>),
        Data(Box<DataObject<'a>>),
        Arg(ArgObject),
        Local(LocalObject),
        Name(NameString),
    }

    from_impl!(<'a>(e: ExpressionOpcode<'a>) -> TermArg<'a> = TermArg::Expression(Box::new(e)));
    from_impl!(<'a>(r: ReferenceExpressionOpcode<'a>) -> TermArg<'a> = ExpressionOpcode::from(r).into());
    from_impl!(<'a>(b: Buffer<'a>) -> TermArg<'a> = ExpressionOpcode::from(b).into());
    // NOTE: Package and VarPackage could be converted through DataObject instead
    from_impl!(<'a>(p: Package<'a>) -> TermArg<'a> = ExpressionOpcode::from(p).into());
    from_impl!(<'a>(p: VarPackage<'a>) -> TermArg<'a> = ExpressionOpcode::from(p).into());
    from_impl!(<'a>(d: DataObject<'a>) -> TermArg<'a> = TermArg::Data(Box::new(d)));
    from_impl!(<'a>(d: ComputationalData<'a>) -> TermArg<'a> = DataObject::from(d).into());
    from_impl!(<'a>(a: ArgObject) -> TermArg<'a> = TermArg::Arg(a));
    from_impl!(<'a>(l: LocalObject) -> TermArg<'a> = TermArg::Local(l));
    from_impl!(<'a>(n: NameString) -> TermArg<'a> = TermArg::Name(n));


    /// Term that attaches a name to its argument.
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum NameSpaceModifier<'a> {
        /// Create a new `alias` for existing named `source` object.
        Alias { source: NameString, alias: NameString },

        /// Attach a name to an anonymous data or reference.
        Name(NameString, DataRefObject<'a>),

        /// Evaluate the contained terms within a new scope.
        Scope(NameString, Vec<TermObject<'a>>),
    }


    /// Term that defines an object with a name.
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum NamedObject<'a> {
        /// Declare fields that can only be accessed after writing to a bank selector
        BankField {
            region_name: NameString,
            bank_name: NameString,
            bank_value: TermArg<'a>,
            flags: FieldFlags,
            elements: Vec<FieldElement<'a>>,
        },

        /// Declare a single-bit field within a buffer
        CreateBitField {
            source_buffer: TermArg<'a>,
            bit_index: TermArg<'a>,
            name: NameString,
        },

        /// Create a one-byte-wide field within a buffer
        CreateByteField {
            source_buffer: TermArg<'a>,
            byte_index: TermArg<'a>,
            name: NameString,
        },

        /// Declare a four-byte-wide field within a buffer
        CreateDWordField {
            source_buffer: TermArg<'a>,
            byte_index: TermArg<'a>,
            name: NameString,
        },

        /// Declare an arbitrary-width field within a buffer
        CreateField {
            source_buffer: TermArg<'a>,
            bit_index: TermArg<'a>,
            num_bits: TermArg<'a>,
            name: NameString,
        },

        /// Declare an eight-byte-wide field within a buffer
        CreateQWordField {
            source_buffer: TermArg<'a>,
            byte_index: TermArg<'a>,
            name: NameString,
        },

        /// Declare a two-byte-wide field within a buffer
        CreateWordField {
            source_buffer: TermArg<'a>,
            byte_index: TermArg<'a>,
            name: NameString,
        },

        /// Allow an ACPI table indexed in the XSDT to be used as a set of fields
        DataTableRegion {
            name: NameString,
            signature: TermArg<'a>,
            oem_id: TermArg<'a>,
            oem_table_id: TermArg<'a>,
        },

        /// Declare a device and its associated fields, methods, and sub-devices
        Device { name: NameString, body: Vec<TermObject<'a>> },

        /// Declare a waitable synchronization object
        Event(NameString),

        /// Declare an object that is defined in another ACPI table
        External { name: NameString, object_type: ObjectType, argument_count: u8 },

        /// Declare a group of fields
        Field {
            region_name: NameString,
            flags: FieldFlags,
            elements: Vec<FieldElement<'a>>,
        },

        /// Declare a group of field that must be accessed by writing to an index
        /// register and then reading/writing from a data register.
        IndexField {
            index_name: NameString,
            data_name: NameString,
            flags: FieldFlags,
            elements: Vec<FieldElement<'a>>,
        },

        /// Declare a control method
        Method { name: NameString, flags: MethodFlags, body: Vec<TermObject<'a>> },

        /// Declare an acquirable mutex
        Mutex { name: NameString, sync_level: u8 },

        /// Declare an address space that can be used by fields
        OperationRegion {
            name: NameString,
            region_space: RegionSpace,
            offset: TermArg<'a>,
            length: TermArg<'a>,
        },

        /// Declare a power resource object
        PowerResource {
            name: NameString,
            system_level: u8,
            resource_order: u16,
            body: Vec<TermObject<'a>>,
        },

        /// Declare a processor and associated register block
        Processor {
            name: NameString,
            id: u8,
            register_block_addr: u32,
            register_block_length: u8,
            body: Vec<TermObject<'a>>,
        },

        /// Declare a thermal zone namespace
        ThermalZone { name: NameString, body: Vec<TermObject<'a>> },
    }


    /// Rules for reading and writing a field.
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct FieldFlags {
        pub access_type: AccessType,
        pub lock: bool,
        pub update_rule: UpdateRule,
    }

    /// Width used to access a field.
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum AccessType {
        Any,
        Byte,
        Word,
        DWord,
        QWord,
        Buffer,
    }

    /// Indicates what to do with unused bits when writing a field.
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum UpdateRule {
        Preserve,
        WriteAsOnes,
        WriteAsZeros,
    }

    /// A substructure of a field.
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum FieldElement<'a> {
        /// A named bitfield.
        Named { name: NameSeg, bit_length: u32 },

        /// Indicates that the next most significant `bit_length` bits of the field are
        /// skipped.
        Reserved { bit_length: u32 },

        /// Sets access information for **following** fields.
        AccessAs(AccessType, AccessAttrib),

        /// Indicates that **following** fields should be accessed with the named
        /// GPIO/Serial descriptor.
        ConnectNamed(NameString),

        /// Indicates that **following** fields should be accessed with the GPIO/Serial
        /// descriptor contained in the given buffer.
        ConnectBuffer(Buffer<'a>),
    }

    /// Additional information about how a field is accessed.
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum AccessAttrib {
        None,
        Quick,
        SendReceive,
        Byte,
        Word,
        Block,
        ProcessCall,
        BlockProcessCall,
        Bytes(u8),
        RawBytes(u8),
        RawProcessBytes(u8),
    }


    /// Information about how to call a method.
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct MethodFlags {
        pub arg_count: u8,
        pub serialized: bool,
        pub sync_level: u8,
    }

    impl MethodFlags {
        pub fn unsynced(arg_count: u8) -> Self {
            Self { arg_count, serialized: false, sync_level: 0 }
        }
    }


    /// Address space used to access a field.
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum RegionSpace {
        SystemMemory,
        SystemIO,
        PCIConfig,
        EmbeddedControl,
        SMBus,
        SystemCMOS,
        PCIBarTarget,
        IPMI,
        GeneralPurposeIO,
        GenericSerialBus,
        PCC,
        OEMDefined(u8),
    }


    /// Terminal operation that does not evaluate to a value, a.k.a. "Type 1 Opcode."
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum StatementOpcode<'a> {
        /// Break from a loop
        Break,

        /// Trigger a debugger breakpoint
        BreakPoint,

        /// Continue to the next loop iteration
        Continue,

        /// Signal an error that requires a system shutdown
        Fatal { fatal_type: u8, code: u32, arg: TermArg<'a> },

        /// Branch on a predicate
        If {
            predicate: TermArg<'a>,
            if_true: Vec<TermObject<'a>>,
            if_false: Vec<TermObject<'a>>,
        },

        /// Load a dynamically-generated SSDT from a field, region, or buffer
        Load { name: NameString, definition_block_handle: SuperName<'a> },

        /// Do nothing
        NoOp,

        /// Send a signal value to a device/processor/zone
        Notify { device_or_zone: SuperName<'a>, value: TermArg<'a> },

        /// Release a held mutex
        Release { mutex: SuperName<'a> },

        /// Clear the signalled state of an event object
        Reset { event: SuperName<'a> },

        /// Exit the current method and yield the given value to the caller
        Return(TermArg<'a>),

        /// Signal to one thread waiting on the event
        Signal { event: SuperName<'a> },

        /// Delay for at least the given milliseconds, releasing the processor
        Sleep { milliseconds: TermArg<'a> },

        /// Delay for at least the given microseconds, but do *not* release the processor
        Stall { microseconds: TermArg<'a> },

        /// Execute a series of statements as long as the predicate is true
        While { predicate: TermArg<'a>, body: Vec<TermObject<'a>> },
    }


    /// Terminal operation that evaluates to a reference.
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum ReferenceExpressionOpcode<'a> {
        /// Create a reference to the given name
        RefOf(SuperName<'a>),

        /// Get the target of a reference
        Deref(TermArg<'a>),

        /// Create a reference to an index within a buffer
        Index { source: TermArg<'a>, index: TermArg<'a>, result: Target<'a> },

        /// Execute a control method
        Invoke(NameString, Vec<TermArg<'a>>),
    }


    /// Terminal operation that evaluates to a value or reference.
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum ExpressionOpcode<'a> {
        RefExpression(ReferenceExpressionOpcode<'a>),
        Buffer(Buffer<'a>),
        Package(Package<'a>),
        VarPackage(VarPackage<'a>),

        /// Try to acquire a mutex, returning *true* if the attempt times out
        Acquire {
            mutex: SuperName<'a>,
            timeout: u16,
        },

        /// Add two integers
        Add(TermArg<'a>, TermArg<'a>, Target<'a>),

        /// Compute a bitwise AND of two integers
        BitwiseAnd(TermArg<'a>, TermArg<'a>, Target<'a>),

        /// Concatenate two strings or buffers
        Concat(TermArg<'a>, TermArg<'a>, Target<'a>),

        /// Concatenate two buffers containing resource templates
        ConcatRes(TermArg<'a>, TermArg<'a>, Target<'a>),

        /// Try to create a reference, returning false if it is not possible
        CondRefOf(SuperName<'a>, Target<'a>),

        /// Update the destination with a copy of the given value, *without* converting
        /// its type. Compare with [`ExpressionOpcode::Store`].
        CopyObject(TermArg<'a>, SimpleName),

        /// Decrement an integer variable
        Decrement(SuperName<'a>),

        /// Perform integer (quotient-remainder) division
        Divide {
            dividend: TermArg<'a>,
            divisor: TermArg<'a>,
            remainder: Target<'a>,
            quotient: Target<'a>,
        },

        /// Get the index of the most-significant set bit in a value
        FindSetLeftBit(TermArg<'a>, Target<'a>),

        /// Get the index of the least-significant set bit in a value
        FindSetRightBit(TermArg<'a>, Target<'a>),

        /// Decode a series of binary-coded decimal nibbles into an integer
        FromBCD(TermArg<'a>, Target<'a>),

        /// Increment an integer variable
        Increment(SuperName<'a>),

        /// Evaluate to true if both arguments are non-zero integers
        LogicalAnd(TermArg<'a>, TermArg<'a>),

        /// Evaluate to true if both values are equal
        Equal(TermArg<'a>, TermArg<'a>),

        /// Evaluate to true if the left value is greater than the right value
        Greater(TermArg<'a>, TermArg<'a>),

        /// Evaluate to true if the left value is greater than or equal to the right
        /// value
        GreaterEqual(TermArg<'a>, TermArg<'a>),

        /// Evaluate to true if the left value is less than the right value
        Less(TermArg<'a>, TermArg<'a>),

        /// Evaluate to true if the left value is less than or equal to the right value
        LessEqual(TermArg<'a>, TermArg<'a>),

        /// Evaluate to true if the value is zero
        LogicalNot(TermArg<'a>),

        /// Evaluate to true if the left value is not equal to the right value
        NotEqual(TermArg<'a>, TermArg<'a>),

        /// Find an ACPI table indexed by the XSDT
        LoadTable {
            signature: TermArg<'a>,
            oem_id: TermArg<'a>,
            oem_table_id: TermArg<'a>,
            root_path: TermArg<'a>,
            parameter_path: TermArg<'a>,
            parameter_data: TermArg<'a>,
        },

        /// Evaluate to true if either of the values is a non-zero integer
        LogicalOr(TermArg<'a>, TermArg<'a>),

        /// Search a package and return the index of the first contained value that
        /// matches both specified conditions
        Match {
            search_package: TermArg<'a>,
            a: (MatchOpcode, TermArg<'a>),
            b: (MatchOpcode, TermArg<'a>),
            start_index: TermArg<'a>,
        },

        /// Copy a slice of a string or buffer.
        Mid {
            source: TermArg<'a>,
            index: TermArg<'a>,
            length: TermArg<'a>,
            result: Target<'a>,
        },

        /// Compute the remainder of dividing the first integer by the second
        Mod(TermArg<'a>, TermArg<'a>, Target<'a>),

        /// Multiply two integers
        Multiply(TermArg<'a>, TermArg<'a>, Target<'a>),

        /// Compute the bitwise NAND of two integers
        Nand(TermArg<'a>, TermArg<'a>, Target<'a>),

        /// Compute the bitwise NOR of two integers
        Nor(TermArg<'a>, TermArg<'a>, Target<'a>),

        /// Invert the bits of an integer
        BitwiseNot(TermArg<'a>, Target<'a>),

        /// Get an integer representing the type of the given value. See [`ObjectType`].
        ObjectType(SuperName<'a>),

        /// Compute the bitwise OR of the two integers
        BitwiseOr(TermArg<'a>, TermArg<'a>, Target<'a>),

        /// Multiply an integer by the specified power of two
        ShiftLeft(TermArg<'a>, TermArg<'a>, Target<'a>),

        /// Divide an integer by the specified power of two
        ShiftRight(TermArg<'a>, TermArg<'a>, Target<'a>),

        /// Get the length of a string, buffer, or package
        SizeOf(SuperName<'a>),

        /// Update the destination with the given value, converting it to the type of the
        /// previous value. Compare with [`ExpressionOpcode::CopyObject`].
        Store(TermArg<'a>, SuperName<'a>),

        /// Subtract an integer from another, ignoring underflow
        Subtract(TermArg<'a>, TermArg<'a>, Target<'a>),

        /// Get the current value of the system timer in 100ns
        Timer,

        /// Encode an integer into a sequence of binary-coded decimal nibbles
        ToBCD(TermArg<'a>, Target<'a>),

        /// Convert a value to a buffer
        ToBuffer(TermArg<'a>, Target<'a>),

        /// Encode a value as an ASCII decimal number
        ToDecimalString(TermArg<'a>, Target<'a>),

        /// Encode a value as an ASCII hexadecimal number
        ToHexString(TermArg<'a>, Target<'a>),

        /// Convert a value to an integer, either by parsing a string or taking the first
        /// bytes of a buffer.
        ToInteger(TermArg<'a>, Target<'a>),

        /// Copy an ASCII string from a buffer into a string value
        ToString {
            source: TermArg<'a>,
            length: TermArg<'a>,
            result: Target<'a>,
        },

        /// Try to wait for another thread to signal an event object, returning true if
        /// the attempt times out
        Wait {
            event: SuperName<'a>,
            timeout: TermArg<'a>,
        },

        /// Compute the bitwise XOR of two integers
        BitwiseXor(TermArg<'a>, TermArg<'a>, Target<'a>),
    }

    from_impl!(
        <'a>(r: ReferenceExpressionOpcode<'a>) -> ExpressionOpcode<'a> =
            ExpressionOpcode::RefExpression(r));
    from_impl!(<'a>(b: Buffer<'a>) -> ExpressionOpcode<'a> = ExpressionOpcode::Buffer(b));
    from_impl!(<'a>(p: Package<'a>) -> ExpressionOpcode<'a> = ExpressionOpcode::Package(p));
    from_impl!(<'a>(p: VarPackage<'a>) -> ExpressionOpcode<'a> = ExpressionOpcode::VarPackage(p));



    /// Type of comparison used for a branch in an [`ExpressionOpcode::Match`] expression.
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum MatchOpcode {
        True,
        Equal,
        LessEqual,
        Less,
        GreaterEqual,
        Greater,
    }

    /// Type ID returned by [`ExpressionOpcode::ObjectType`].
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum ObjectType {
        Uninitialized,
        Integer,
        String,
        Buffer,
        Package,
        FieldUnit,
        Device,
        Event,
        Method,
        Mutex,
        OperationRegion,
        PowerResource,
        Processor,
        ThermalZone,
        BufferField,
        DDBHandle,
        DebugObject,
    }
}



/// Special symbols
pub mod misc {
    /// Symbol for a positional argument passed to the current method.
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum ArgObject {
        Arg0,
        Arg1,
        Arg2,
        Arg3,
        Arg4,
        Arg5,
        Arg6,
    }

    /// Symbol for a variable local to the current method.
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum LocalObject {
        Local0,
        Local1,
        Local2,
        Local3,
        Local4,
        Local5,
        Local6,
        Local7,
    }

    /// Symbol representing debugger output device.
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct DebugObject;
}
