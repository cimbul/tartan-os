//! Abstract syntax tree for ACPI Machine Language


#![allow(clippy::module_name_repetitions)]


pub mod parse;


macro_rules! from_impl {
    [$( <$a:lifetime> )? ($x:ident: $from:ty) -> $to:ty = $imp:expr] => {
        impl$(<$a>)? From<$from> for $to {
            fn from($x: $from) -> $to { $imp }
        }
    };
}


/// Names of objects, arguments, and references
pub mod name {
    use alloc::boxed::Box;
    use alloc::vec::Vec;
    use super::misc::{ArgObject, LocalObject};
    use super::term::ReferenceExpressionOpcode;


    /// Four-character name segment, allowing underscores, uppercase letters, and digits
    /// (except at the beginning).
    ///
    /// The ASL compiler uses underscores to pad the end of names shorter than 4 chars.
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct NameSeg(pub [u8; 4]);

    from_impl!((n: &[u8; 4]) -> NameSeg = NameSeg(*n));


    /// Fully qualified object path, either absolute or relative.
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct NameString {
        pub anchor: PathAnchor,
        pub path: Vec<NameSeg>,
    }

    impl NameString {
        pub fn new(path: &[NameSeg]) -> Self {
            NameString::new_parent(0, path)
        }

        pub fn new_root(path: &[NameSeg]) -> Self {
            NameString { path: path.into(), anchor: PathAnchor::Root }
        }

        pub fn new_parent(n: usize, path: &[NameSeg]) -> Self {
            NameString { path: path.into(), anchor: PathAnchor::Parent(n) }
        }
    }

    from_impl!((n: NameSeg) -> NameString = NameString::new(&[n]));
    from_impl!((n: &[u8; 4]) -> NameString = NameSeg::from(n).into());


    /// Indicates whether a name is absolute or relative to the current or parent scope.
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum PathAnchor {
        Root,
        Parent(usize),
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
    use alloc::vec::Vec;
    use super::name::NameString;
    use super::term::TermArg;

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

    /// Group of related data elements, optionally inititalized in whole or in part.
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
    use alloc::boxed::Box;
    use alloc::vec::Vec;
    use super::name::{NameSeg, NameString, SimpleName, SuperName, Target};
    use super::data::{ComputationalData, Buffer, DataRefObject, DataObject, Package, VarPackage};
    use super::misc::{ArgObject, LocalObject};

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
            name: NameString
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
        Device {
            name: NameString,
            body: Vec<TermObject<'a>>,
        },

        /// Declare a waitable synchronization object
        Event(NameString),

        /// Declare an object that is defined in another ACPI table
        External {
            name: NameString,
            object_type: ObjectType,
            argument_count: u8,
        },

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
        Method {
            name: NameString,
            flags: MethodFlags,
            body: Vec<TermObject<'a>>,
        },

        /// Declare an acquirable mutex
        Mutex {
            name: NameString,
            sync_level: u8,
        },

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
        ThermalZone {
            name: NameString,
            body: Vec<TermObject<'a>>,
        },
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
        // DefBreak        := BreakOp
        // BreakOp         := 0xA5
        Break,

        // DefBreakPoint   := BreakPointOp
        // BreakPointOp    := 0xCC
        BreakPoint,

        // DefContinue     := ContinueOp
        // ContinueOp      := 0x9F
        Continue,

        // DefFatal        := FatalOp FatalType FatalCode FatalArg
        // FatalOp         := ExtOpPrefix 0x32
        // FatalType       := ByteData
        // FatalCode       := DWordData
        // FatalArg        := TermArg => Integer
        Fatal { fatal_type: u8, code: u32, arg: TermArg<'a> },

        // DefIfElse       := IfOp PkgLength Predicate TermList DefElse
        // IfOp            := 0xA0
        // Predicate       := TermArg => Integer
        // DefElse         := Nothing | <ElseOp PkgLength TermList>
        // ElseOp          := 0xA1
        If {
            predicate: TermArg<'a>,
            if_true: Vec<TermObject<'a>>,
            if_false: Option<Vec<TermObject<'a>>>,
        },

        // DefLoad         := LoadOp NameString DDBHandleObject
        // LoadOp          := ExtOpPrefix 0x20
        // DDBHandleObject := SuperName
        Load { name: NameString, definition_block_handle: SuperName<'a> },

        // DefNoop         := NoopOp
        // NoopOp          := 0xA3
        NoOp,

        // DefNotify       := NotifyOp NotifyObject NotifyValue
        // NotifyOp        := 0x86
        // NotifyObject    := SuperName => ThermalZone | Processor | Device
        // NotifyValue     := TermArg => Integer
        Notify { device_or_zone: SuperName<'a>, value: TermArg<'a> },

        // DefRelease      := ReleaseOp MutexObject
        // ReleaseOp       := ExtOpPrefix 0x27
        // MutexObject     := SuperName
        Release { mutex: SuperName<'a> },

        // DefReset        := ResetOp EventObject
        // ResetOp         := ExtOpPrefix 0x26
        // EventObject     := SuperName
        Reset { event: SuperName<'a> },

        // DefReturn       := ReturnOp ArgObject
        // ReturnOp        := 0xA4
        // ArgObject       := TermArg => DataRefObject
        Return(TermArg<'a>),

        // DefSignal       := SignalOp EventObject
        // SignalOp        := ExtOpPrefix 0x24
        Signal { event: SuperName<'a> },

        // DefSleep        := SleepOp MsecTime
        // SleepOp         := ExtOpPrefix 0x22
        // MsecTime        := TermArg => Integer
        Sleep { milliseconds: TermArg<'a> },

        // DefStall        := StallOp UsecTime
        // StallOp         := ExtOpPrefix 0x21
        // UsecTime        := TermArg => ByteData
        Stall { microseconds: TermArg<'a> },

        // DefWhile        := WhileOp PkgLength Predicate TermList
        // WhileOp         := 0xA2
        While { predicate: TermArg<'a>, body: Vec<TermObject<'a>> },
    }


    /// Terminal operation that evaluates to a reference.
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum ReferenceExpressionOpcode<'a> {
        // DefRefOf            := RefOfOp SuperName
        // RefOfOp             := 0x71
        DefRefOf(SuperName<'a>),

        // DefDerefOf          := DerefOfOp ObjReference
        // DerefOfOp           := 0x83
        // ObjReference        := TermArg => ??ObjectReference | String
        DefDerefOf(TermArg<'a>),

        // DefIndex            := IndexOp BuffPkgStrObj IndexValue Target
        // IndexOp             := 0x88
        // BuffPkgStrObj       := TermArg => Buffer, Package or String
        // IndexValue          := TermArg => Integer
        DefIndex { source: TermArg<'a>, index: TermArg<'a>, result: Target<'a> },

        // MethodInvocation := NameString TermArgList
        MethodInvocation { source: NameString, args: Vec<TermArg<'a>> },
    }


    /// Terminal operation that evaluates to a value or reference.
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum ExpressionOpcode<'a> {
        RefExpression(ReferenceExpressionOpcode<'a>),
        Buffer(Buffer<'a>),
        Package(Package<'a>),
        VarPackage(VarPackage<'a>),

        // DefAcquire          := AcquireOp MutexObject Timeout
        // AcquireOp           := ExtOpPrefix 0x23
        // Timeout             := WordData
        Acquire { mutex: SuperName<'a>, timeout: u16 },

        // DefAdd              := AddOp Operand Operand Target
        // AddOp               := 0x72
        // Operand             := TermArg => Integer
        Add(TermArg<'a>, TermArg<'a>, Target<'a>),

        // DefAnd              := AndOp Operand Operand Target
        // AndOp               := 0x7B
        BitwiseAnd(TermArg<'a>, TermArg<'a>, Target<'a>),

        // DefConcat           := ConcatOp Data Data Target
        // ConcatOp            := 0x73
        // Data                := TermArg => ComputationalData
        Concat(TermArg<'a>, TermArg<'a>, Target<'a>),

        // DefConcatRes        := ConcatResOp BufData BufData Target
        // ConcatResOp         := 0x84
        // BufData             := TermArg => Buffer
        ConcatRes(TermArg<'a>, TermArg<'a>, Target<'a>),

        // DefCondRefOf        := CondRefOfOp SuperName Target
        // CondRefOfOp         := ExtOpPrefix 0x12
        CondRefOf(SuperName<'a>, Target<'a>),

        // DefCopyObject       := CopyObjectOp TermArg SimpleName
        // CopyObjectOp        := 0x9D
        CopyObject(TermArg<'a>, SimpleName),

        // DefDecrement        := DecrementOp SuperName
        // DecrementOp         := 0x76
        Decrement(SuperName<'a>),

        // DefDivide           := DivideOp Dividend Divisor Remainder Quotient
        // DivideOp            := 0x78
        // Dividend            := TermArg => Integer
        // Divisor             := TermArg => Integer
        // Remainder           := Target
        // Quotient            := Target
        Divide {
            dividend: TermArg<'a>,
            divisor: TermArg<'a>,
            remainder: Target<'a>,
            quotient: Target<'a>,
        },

        // DefFindSetLeftBit   := FindSetLeftBitOp Operand Target
        // FindSetLeftBitOp    := 0x81
        FindSetLeftBit(TermArg<'a>, Target<'a>),

        // DefFindSetRightBit  := FindSetRightBitOp Operand Target
        // FindSetRightBitOp   := 0x82
        FindSetRightBit(TermArg<'a>, Target<'a>),

        // DefFromBCD          := FromBCDOp BCDValue Target
        // FromBCDOp           := ExtOpPrefix 0x28
        // BCDValue            := TermArg => Integer
        FromBCD(TermArg<'a>, Target<'a>),

        // DefIncrement        := IncrementOp SuperName
        // IncrementOp         := 0x75
        Increment(SuperName<'a>),

        // DefLAnd             := LandOp Operand Operand
        // LandOp              := 0x90
        LogicalAnd(TermArg<'a>, TermArg<'a>),

        // DefLEqual           := LequalOp Operand Operand
        // LequalOp            := 0x93
        Equal(TermArg<'a>, TermArg<'a>),

        // DefLGreater         := LgreaterOp Operand Operand
        // LgreaterOp          := 0x94
        Greater(TermArg<'a>, TermArg<'a>),

        // DefLGreaterEqual    := LgreaterEqualOp Operand Operand
        // LgreaterEqualOp     := LnotOp LlessOp
        GreaterEqual(TermArg<'a>, TermArg<'a>),

        // DefLLess            := LlessOp Operand Operand
        // LlessOp             := 0x95
        Less(TermArg<'a>, TermArg<'a>),

        // DefLLessEqual       := LlessEqualOp Operand Operand
        // LlessEqualOp        := LnotOp LgreaterOp
        LessEqual(TermArg<'a>, TermArg<'a>),

        // DefLNot             := LnotOp Operand
        // LnotOp              := 0x92
        LogicalNot(TermArg<'a>),

        // DefLNotEqual        := LnotEqualOp Operand Operand
        // LnotEqualOp         := LnotOp LequalOp
        NotEqual(TermArg<'a>, TermArg<'a>),

        // DefLoadTable        := LoadTableOp TermArg TermArg TermArg TermArg TermArg TermArg
        // LoadTableOp         := ExtOpPrefix 0x1F
        LoadTable {
            signature: TermArg<'a>,
            oem_id: TermArg<'a>,
            oem_table_id: TermArg<'a>,
            root_path: TermArg<'a>,
            parameter_path: TermArg<'a>,
            parameter_data: TermArg<'a>,
        },

        // DefLOr              := LorOp Operand Operand
        // LorOp               := 0x91
        LogicalOr(TermArg<'a>, TermArg<'a>),

        // DefMatch            := MatchOp SearchPkg MatchOpcode Operand MatchOpcode Operand StartIndex
        // MatchOp             := 0x89
        // SearchPkg           := TermArg => Package
        // StartIndex          := TermArg => Integer
        Match {
            search_package: TermArg<'a>,
            a: (MatchOpcode, TermArg<'a>),
            b: (MatchOpcode, TermArg<'a>),
            start_index: TermArg<'a>,
        },

        // DefMid              := MidOp MidObj TermArg TermArg Target
        // MidOp               := 0x9E
        // MidObj              := TermArg => Buffer | String
        Mid {
            source: TermArg<'a>,
            index: TermArg<'a>,
            length: TermArg<'a>,
            result: Target<'a>,
        },

        // DefMod              := ModOp Dividend Divisor Target
        // ModOp               := 0x85
        Mod(TermArg<'a>, TermArg<'a>, Target<'a>),

        // DefMultiply         := MultiplyOp Operand Operand Target
        // MultiplyOp          := 0x77
        Multiply(TermArg<'a>, TermArg<'a>, Target<'a>),

        // DefNAnd             := NandOp Operand Operand Target
        // NandOp              := 0x7C
        Nand(TermArg<'a>, TermArg<'a>, Target<'a>),

        // DefNOr              := NorOp Operand Operand Target
        // NorOp               := 0x7E
        Nor(TermArg<'a>, TermArg<'a>, Target<'a>),

        // DefNot              := NotOp Operand Target
        // NotOp               := 0x80
        BitwiseNot(TermArg<'a>, Target<'a>),

        // DefObjectType       := ObjectTypeOp <SimpleName | DebugObj |
        //                        DefRefOf | DefDerefOf | DefIndex>
        // ObjectTypeOp        := 0x8E
        //
        // NOTE: SuperName includes MethodInvocation, which is *not* legal for the
        // ObjectType operator. It is otherwise identical to the grammar above.
        ObjectType(SuperName<'a>),

        // DefOr               := OrOp Operand Operand Target
        // OrOp                := 0x7D
        BitwiseOr(TermArg<'a>, TermArg<'a>, Target<'a>),

        // DefShiftLeft        := ShiftLeftOp Operand ShiftCount Target
        // ShiftLeftOp         := 0x79
        // ShiftCount          := TermArg => Integer
        ShiftLeft(TermArg<'a>, TermArg<'a>, Target<'a>),

        // DefShiftRight       := ShiftRightOp Operand ShiftCount Target
        // ShiftRightOp        := 0x7A
        ShiftRight(TermArg<'a>, TermArg<'a>, Target<'a>),

        // DefSizeOf           := SizeOfOp SuperName
        // SizeOfOp            := 0x87
        SizeOf(SuperName<'a>),

        // DefStore            := StoreOp TermArg SuperName
        // StoreOp             := 0x70
        Store(TermArg<'a>, SuperName<'a>),

        // DefSubtract         := SubtractOp Operand Operand Target
        // SubtractOp          := 0x74
        Subtract(TermArg<'a>, TermArg<'a>, Target<'a>),

        // DefTimer            := TimerOp
        // TimerOp             := 0x5B 0x33
        Timer,

        // DefToBCD            := ToBCDOp Operand Target
        // ToBCDOp             := ExtOpPrefix 0x29
        ToBCD(TermArg<'a>, Target<'a>),

        // DefToBuffer         := ToBufferOp Operand Target
        // ToBufferOp          := 0x96
        ToBuffer(TermArg<'a>, Target<'a>),

        // DefToDecimalString  := ToDecimalStringOp Operand Target
        // ToDecimalStringOp   := 0x97
        ToDecimalString(TermArg<'a>, Target<'a>),

        // DefToHexString      := ToHexStringOp Operand Target
        // ToHexStringOp       := 0x98
        ToHexString(TermArg<'a>, Target<'a>),

        // DefToInteger        := ToIntegerOp Operand Target
        // ToIntegerOp         := 0x99
        ToInteger(TermArg<'a>, Target<'a>),

        // DefToString         := ToStringOp TermArg LengthArg Target
        // LengthArg           := TermArg => Integer
        // ToStringOp          := 0x9C
        ToString { source: TermArg<'a>, length: TermArg<'a>, result: Target<'a>},

        // DefWait             := WaitOp EventObject Operand
        // WaitOp              := ExtOpPrefix 0x25
        Wait { event: SuperName<'a>, timeout: TermArg<'a> },

        // DefXOr              := XorOp Operand Operand Target
        // XorOp               := 0x7F
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
