use alloc::vec::Vec;


// 1. Table and Table Header
//
// AMLCode         := DefBlockHeader TermList
//
// Everything in DefBlockHeader just repeats the fixed DescriptionHeader struct used in
// other tables, so there's no reason to write a parser for it.
//
// DefBlockHeader  := TableSignature TableLength SpecCompliance CheckSum OemID
//                    OemTableID OemRevision CreatorID CreatorRevision
//
// TableSignature  := DWordData // As defined in section 5.2.3.
// TableLength     := DWordData
//     // Length of the table in bytes including
//     // the block header.
// SpecCompliance  := ByteData // The revision of the structure.
// CheckSum        := ByteData // Byte checksum of the entire table.
// OemID           := ByteData(6)
//     // OEM ID of up to 6 characters. If the OEM
//     // ID is shorter than 6 characters, it
//     // can be terminated with a NULL
//     // character.
// OemTableID      := ByteData(8)
//     // OEM Table ID of up to 8 characters. If
//     // the OEM Table ID is shorter than 8
//     // characters, it can be terminated with
//     // a NULL character.
// OemRevision     := DWordData // OEM Table Revision.
// CreatorID       := DWordData // Vendor ID of the ASL compiler.
// CreatorRevision := DWordData // Revision of the ASL compiler.


//
// 2. Name Objects
//
pub mod name {
    use super::*;
    use super::misc::{ArgObject, LocalObject};
    use super::term::ReferenceExpressionOpcode;

    // LeadNameChar     := ‘A’-‘Z’ | ‘_’
    // DigitChar        := ‘0’-‘9’
    // NameChar         := DigitChar | LeadNameChar
    //
    // NameSeg          := <LeadNameChar NameChar NameChar NameChar>
    //     // Notice that NameSegs shorter than 4 characters are filled with
    //     // trailing underscores (‘_’s).
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct NameSeg(pub [u8; 4]);

    // NameString       := <RootChar NamePath> | <PrefixPath NamePath>
    // PrefixPath       := Nothing | <‘^’ PrefixPath>
    // NamePath         := NameSeg | DualNamePath | MultiNamePath | NullName
    //
    // RootChar         := ‘\’
    // ParentPrefixChar := ‘^’
    //
    // NullName         := 0x00
    // DualNamePath     := DualNamePrefix NameSeg NameSeg
    // DualNamePrefix   := 0x2E
    // MultiNamePath    := MultiNamePrefix SegCount NameSeg(SegCount)
    // MultiNamePrefix  := 0x2F
    //
    // SegCount         := ByteData
    //     // SegCount can be from 1 to 255. For example: MultiNamePrefix(35) is encoded
    //     // as 0x2f 0x23 and followed by 35 NameSegs. So, the total encoding length will
    //     // be 1 + 1 + 35*4 = 142. Notice that: DualNamePrefix NameSeg NameSeg has a
    //     // smaller encoding than the encoding of: MultiNamePrefix(2) NameSeg NameSeg
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct NameString {
        pub anchor: PathAnchor,
        pub path: Vec<NameSeg>,
    }

    impl NameString {
        pub fn new(path: Vec<NameSeg>) -> Self {
            NameString::new_parent(0, path)
        }

        pub fn new_root(path: Vec<NameSeg>) -> Self {
            NameString { path, anchor: PathAnchor::Root }
        }

        pub fn new_parent(n: usize, path: Vec<NameSeg>) -> Self {
            NameString { path, anchor: PathAnchor::Parent(n) }
        }
    }

    // See grammar for NameString
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum PathAnchor {
        Root,
        Parent(usize),
    }

    // SimpleName       := NameString | ArgObj | LocalObj
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum SimpleName {
        Name(NameString),
        Arg(ArgObject),
        Local(LocalObject),
    }

    // SuperName        := SimpleName | DebugObj | Type6Opcode
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum SuperName<'a> {
        Name(SimpleName),
        Debug,
        Reference(&'a ReferenceExpressionOpcode<'a>),
    }

    // Target           := SuperName | NullName
    pub type Target<'a> = Option<SuperName<'a>>;
}



//
// 3. Data Objects
//
pub mod data {
    use super::*;
    use super::name::NameString;
    use super::term::TermArg;

    // ComputationalData := ByteConst | WordConst | DWordConst | QWordConst | String |
    //                      ConstObj | RevisionOp | DefBuffer
    //
    // ByteConst         := BytePrefix ByteData
    // BytePrefix        := 0x0A
    // WordConst         := WordPrefix WordData
    // WordPrefix        := 0x0B
    // DWordConst        := DWordPrefix DWordData
    // DWordPrefix       := 0x0C
    // QWordConst        := QWordPrefix QWordData
    // QWordPrefix       := 0x0E
    // String            := StringPrefix AsciiCharList NullChar
    // StringPrefix      := 0x0D
    //
    // ConstObj          := ZeroOp | OneOp | OnesOp
    // ByteList          := Nothing | <ByteData ByteList>
    // ByteData          := 0x00 - 0xFF
    // WordData          := ByteData[0:7] ByteData[8:15]
    //     // 0x0000-0xFFFF
    // DWordData         := WordData[0:15] WordData[16:31]
    //     // 0x00000000-0xFFFFFFFF
    // QWordData         := DWordData[0:31] DWordData[32:63]
    //     // 0x0000000000000000-0xFFFFFFFFFFFFFFFF
    // AsciiCharList     := Nothing | <AsciiChar AsciiCharList>
    // AsciiChar         := 0x01 - 0x7F
    // NullChar          := 0x00
    // ZeroOp            := 0x00
    // OneOp             := 0x01
    // OnesOp            := 0xFF
    // RevisionOp        := ExtOpPrefix 0x30
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
        Buffer(Buffer),
    }

    // DefBuffer           := BufferOp PkgLength BufferSize ByteList
    // BufferOp            := 0x11
    // BufferSize          := TermArg => Integer
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct Buffer {
        pub size: u32,
        pub initializer: Vec<u8>,
    }

    // DefPackage          := PackageOp PkgLength NumElements PackageElementList
    // PackageOp           := 0x12
    // NumElements         := ByteData
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct Package<'a> {
        pub count: u8,
        pub initializers: Vec<PackageElement<'a>>,
    }

    // DefVarPackage       := VarPackageOp PkgLength VarNumElements PackageElementList
    // VarPackageOp        := 0x13
    // VarNumElements      := TermArg => Integer
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct VarPackage<'a> {
        pub count: TermArg<'a>,
        pub initializers: Vec<PackageElement<'a>>,
    }

    // PackageElementList  := Nothing | <PackageElement PackageElementList>
    // PackageElement      := DataRefObject | NameString
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum PackageElement<'a> {
        Ref(DataRefObject<'a>),
        Name(NameString),
    }

    // DataObject        := ComputationalData | DefPackage | DefVarPackage
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum DataObject<'a> {
        Data(ComputationalData<'a>),
        Package(Package<'a>),
        VarPackage(VarPackage<'a>),
    }

    // DataRefObject     := DataObject | ObjectReference | DDBHandle
    //
    // From ASL grammar (19.2.4):
    //
    // DDBHandle       := Integer
    // ObjectReference := Integer
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum DataRefObject<'a> {
        Data(DataObject<'a>),
        ObjectReference(u64),
        DDBHandle(u64),
    }
}



//
// 5. Term Objects
//
pub mod term {
    use super::*;
    use super::name::{NameSeg, NameString, SimpleName, SuperName, Target};
    use super::data::{Buffer, DataRefObject, DataObject, Package, VarPackage};
    use super::misc::{ArgObject, LocalObject};

    // Object           := NameSpaceModifierObj | NamedObj
    // TermObj          := Object | Type1Opcode | Type2Opcode
    // TermList         := Nothing | <TermObj TermList>
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum TermObject<'a> {
        Modifier(&'a NameSpaceModifier<'a>),
        Named(&'a NamedObject<'a>),
        Statement(&'a StatementOpcode<'a>),
        Expression(&'a ExpressionOpcode<'a>),
    }


    // TermArg          := Type2Opcode | DataObject | ArgObj | LocalObject
    // TermArgList      := Nothing | <TermArg TermArgList>
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum TermArg<'a> {
        Expression(&'a ExpressionOpcode<'a>),
        Data(&'a DataObject<'a>),
        Arg(ArgObject),
        Local(LocalObject),
    }


    //
    // 5.1 NameString Modifier Objects,
    //

    // NameSpaceModifierObj := DefAlias | DefName | DefScope
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum NameSpaceModifier<'a> {
        // DefAlias             := AliasOp NameString NameString
        // AliasOp              := 0x06
        Alias { source: NameString, alias: NameString },

        // DefName              := NameOp NameString DataRefObject
        // NameOp               := 0x08
        Name(NameString, DataRefObject<'a>),

        // DefScope             := ScopeOp PkgLength NameString TermList
        // ScopeOp              := 0x10
        Scope(NameString, Vec<TermObject<'a>>),
    }


    //
    // 5.2 Named Objects
    //
    // NamedObj := DefBankField | DefCreateBitField | DefCreateByteField | DefCreateDWordField |
    //     DefCreateField | DefCreateQWordField | DefCreateWordField | DefDataRegion |
    //     DefExternal | DefOpRegion | DefPowerRes | DefProcessor | DefThermalZone
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum NamedObject<'a> {
        // DefBankField := BankFieldOp PkgLength NameString NameString BankValue FieldFlags FieldList
        // BankFieldOp  := ExtOpPrefix 0x87
        // BankValue    := TermArg => Integer
        BankField {
            region_name: NameString,
            bank_name: NameString,
            bank_value: TermArg<'a>,
            flags: FieldFlags,
            fields: Vec<FieldElement>,
        },

        // DefCreateBitField    := CreateBitFieldOp SourceBuff BitIndex NameString
        // CreateBitFieldOp     := 0x8D
        // SourceBuff           := TermArg => Buffer
        // BitIndex             := TermArg => Integer
        CreateBitField {
            source_buffer: TermArg<'a>,
            bit_index: TermArg<'a>,
            name: NameString,
        },

        // DefCreateByteField   := CreateByteFieldOp SourceBuff ByteIndex NameString
        // CreateByteFieldOp    := 0x8C
        // ByteIndex            := TermArg => Integer
        CreateByteField {
            source_buffer: TermArg<'a>,
            byte_index: TermArg<'a>,
            name: NameString,
        },

        // DefCreateDWordField  := CreateDWordFieldOp SourceBuff ByteIndex NameString
        // CreateDWordFieldOp   := 0x8A
        CreateDWordField {
            source_buffer: TermArg<'a>,
            byte_index: TermArg<'a>,
            name: NameString,
        },

        // DefCreateField       := CreateFieldOp SourceBuff BitIndex NumBits NameString
        // CreateFieldOp        := ExtOpPrefix 0x13
        // NumBits              := TermArg => Integer
        CreateField {
            source_buffer: TermArg<'a>,
            bit_index: TermArg<'a>,
            num_bits: TermArg<'a>,
            name: NameString
        },

        // DefCreateQWordField  := CreateQWordFieldOp SourceBuff ByteIndex NameString
        // CreateQWordFieldOp   := 0x8F
        CreateQWordField {
            source_buffer: TermArg<'a>,
            byte_index: TermArg<'a>,
            name: NameString,
        },

        // DefCreateWordField   := CreateWordFieldOp SourceBuff ByteIndex NameString
        // CreateWordFieldOp    := 0x8B
        CreateWordField {
            source_buffer: TermArg<'a>,
            byte_index: TermArg<'a>,
            name: NameString,
        },

        // DefDataRegion        := DataRegionOp NameString TermArg TermArg TermArg
        // DataRegionOp         := ExOpPrefix 0x88
        DataTableRegion {
            name: NameString,
            signature: TermArg<'a>,
            oem_id: TermArg<'a>,
            oem_table_id: TermArg<'a>,
        },

        // DefDevice            := DeviceOp PkgLength NameString TermList
        // DeviceOp             := ExtOpPrefix 0x82
        Device {
            name: NameString,
            body: Vec<TermObject<'a>>,
        },

        // DefEvent             := EventOp NameString
        // EventOp              := ExtOpPrefix 0x02
        Event(NameString),

        // DefExternal          := ExternalOp NameString ObjectType ArgumentCount
        // ExternalOp           := 0x15
        // ObjectType           := ByteData
        // ArgumentCount        := ByteData (0 – 7)
        External {
            name: NameString,
            object_type: ObjectType,
            argument_count: u8,
        },

        // DefField             := FieldOp PkgLength NameString FieldFlags FieldList
        // FieldOp              := ExtOpPrefix 0x81
        Field {
            region_name: NameString,
            flags: FieldFlags,
            elements: Vec<FieldElement>,
        },

        // DefIndexField        := IndexFieldOp PkgLength NameString NameString FieldFlags FieldList
        // IndexFieldOp         := ExtOpPrefix 0x86
        IndexField {
            index_name: NameString,
            data_name: AccessType,
            flags: FieldFlags,
            elements: Vec<FieldElement>,
        },

        // DefMethod            := MethodOp PkgLength NameString MethodFlags TermList
        // MethodOp             := 0x14
        Method {
            name: NameString,
            flags: MethodFlags,
            body: Vec<TermObject<'a>>,
        },

        // DefMutex             := MutexOp NameString SyncFlags
        // MutexOp              := ExtOpPrefix 0x01
        // SyncFlags            := ByteData
        //     // bit 0-3: SyncLevel (0x00-0x0f)
        //     // bit 4-7: Reserved (must be 0)
        Mutex {
            name: NameString,
            sync_level: u8,
        },

        // DefOpRegion          := OpRegionOp NameString RegionSpace RegionOffset RegionLen
        // OpRegionOp           := ExtOpPrefix 0x80
        // RegionOffset         := TermArg => Integer
        // RegionLen            := TermArg => Integer
        OperationRegion {
            name: NameString,
            region_space: RegionSpace,
            offset: TermArg<'a>,
            length: TermArg<'a>,
        },

        // DefPowerRes          := PowerResOp PkgLength NameString SystemLevel ResourceOrder TermList
        // PowerResOp           := ExtOpPrefix 0x84
        // SystemLevel          := ByteData
        // ResourceOrder        := WordData
        PowerResource {
            name: NameString,
            system_level: u8,
            resource_order: u16,
            body: Vec<TermObject<'a>>,
        },

        // DefProcessor         := ProcessorOp PkgLength NameString ProcID PblkAddr PblkLen TermList
        // ProcessorOp          := ExtOpPrefix 0x83
        // ProcID               := ByteData
        // PblkAddr             := DWordData
        // PblkLen              := ByteData
        Processor {
            name: NameString,
            id: u8,
            register_block_addr: u32,
            register_block_length: u8,
            body: Vec<TermObject<'a>>,
        },

        // DefThermalZone       := ThermalZoneOp PkgLength NameString TermList
        // ThermalZoneOp        := ExtOpPrefix 0x85
        ThermalZone {
            name: NameString,
            body: Vec<TermObject<'a>>,
        },
    }

    // FieldFlags   := ByteData
    //     // bit 0-3: AccessType
    //     //          0    AnyAcc
    //     //          1    ByteAcc
    //     //          2    WordAcc
    //     //          3    DWordAcc
    //     //          4    QWordAcc
    //     //          5    BufferAcc
    //     //          6    Reserved
    //     //          7-15 Reserved
    //     // bit 4:   LockRule
    //     //          0    NoLock
    //     //          1    Lock
    //     // bit 5-6: UpdateRule
    //     //          0    Preserve
    //     //          1    WriteAsOnes
    //     //          2    WriteAsZeros
    //     // bit 7:   Reserved (must be 0)
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct FieldFlags {
        pub access_type: AccessType,
        pub lock: bool,
        pub update_rule: UpdateRule,
    }

    // See grammar of FieldFlags.
    //
    // This does **not** match the "AccessType" production in the AML grammar, which is
    // really a hybrid between the AccessType and AccessAttribute productions from the ASL
    // grammar. See the AccessAttrib struct for the AML grammar.
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum AccessType {
        Any,
        Byte,
        Word,
        DWord,
        Buffer,
    }

    // See grammar of FieldFlags
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum UpdateRule {
        Preserve,
        WriteAsOnes,
        WriteAsZeros,
    }

    // FieldList     := Nothing | <FieldElement FieldList>
    // FieldElement  := NamedField | ReservedField | AccessField | ExtendedAccessField |
    //                  ConnectField
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum FieldElement {
        // NamedField           := NameSeg PkgLength
        Named { name: NameSeg, bit_length: u32 },

        // ReservedField        := 0x00 PkgLength
        //
        // NOTE: Although the corresponding ASL "Offset" grammar uses an *absolute*
        // *byte* offset, the ACPICA compiler seems to translate it into *relative* *bit*
        // offset when it outputs AML.
        Reserved { bit_length: u32 },

        // This combines two alternatives in the AML grammar that encode the same
        // information. See the AccessAttrib struct.
        //
        // AccessField          := 0x01 AccessType AccessAttrib
        // ExtendedAccessField  := 0x03 AccessType ExtendedAccessAttrib ??AccessLength
        //
        // The ACPICA AML parser expects "AccessLength" to be a byte.
        // See `source/components/parser/psargs.c`.
        AccessAs(AccessType, AccessAttrib),

        // ConnectField         := <0x02 NameString> | <0x02 ??BufferData>
        //
        // The ACPICA parser expects "BufferData" to be a DefBuffer op. Splitting this
        // into two enum variants to avoid another level of indirection.
        ConnectNamed(NameString),
        ConnectBuffer(Buffer),
    }

    // The AML spec is kind of a mess here, and the same information can apparently be
    // encoded in multiple ways. This enum reflects the ASL grammar. I don't know whether
    // the compiler prefers one representation over the other, but we'll assume there is
    // no real difference.
    //
    // AccessType    := ByteData
    //     // Bits 0:3 - Same as AccessType bits of FieldFlags.
    //     // Bits 4:5 - Reserved
    //     // Bits 7:6 - 0 = AccessAttrib = Normal Access Attributes
    //     //            1 = AccessAttrib = AttribBytes (x)
    //     //            2 = AccessAttrib = AttribRawBytes (x)
    //     //            3 = AccessAttrib = AttribRawProcessBytes (x)
    //     //            x' is encoded as bits 0:7 of the AccessAttrib byte.
    //
    // AccessAttrib  := ByteData
    //     // If AccessType is BufferAcc for the SMB or
    //     // GPIO OpRegions, AccessAttrib can be one of
    //     // the following values:
    //     //    0x02   AttribQuick
    //     //    0x04   AttribSendReceive
    //     //    0x06   AttribByte
    //     //    0x08   AttribWord
    //     //    0x0A   AttribBlock
    //     //    0x0C   AttribProcessCall
    //     //    0x0D   AttribBlockProcessCall
    //
    // ExtendedAccessAttrib := ByteData
    //     // 0x0B AttribBytes
    //     // 0x0E AttribRawBytes
    //     // 0x0F AttribRawProcess
    //
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum AccessAttrib {
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

    // MethodFlags          := ByteData
    //     // bit 0-2: ArgCount (0-7)
    //     // bit 3:   SerializeFlag
    //     //          0 NotSerialized
    //     //          1 Serialized
    //     // bit 4-7: SyncLevel (0x00-0x0f)
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct MethodFlags {
        pub arg_count: u8,
        pub serialized: bool,
        pub sync_level: u8,
    }

    // RegionSpace          := ByteData
    //     // 0x00       SystemMemory
    //     // 0x01       SystemIO
    //     // 0x02       PCI_Config
    //     // 0x03       EmbeddedControl
    //     // 0x04       SMBus
    //     // 0x05       SystemCMOS
    //     // 0x06       PciBarTarget
    //     // 0x07       IPMI
    //     // 0x08       GeneralPurposeIO
    //     // 0x09       GenericSerialBus
    //     // 0x0A       PCC
    //     // 0x80-0xFF: OEM Defined
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


    //
    // 5.3 Type 1 Opcodes
    //
    // From ASL grammar (19.2.5):
    //
    // A Type 1 opcode term does not return a value and can only be used standalone on a
    // line of ASL code. Since these opcodes do not return a value they cannot be used as
    // a term in an expression.
    //
    //
    // Type1Opcode     := DefBreak | DefBreakPoint | DefContinue | DefFatal | DefIfElse |
    //                    DefLoad | DefNoop | DefNotify | DefRelease | DefReset | DefReturn |
    //                    DefSignal | DefSleep | DefStall | DefWhile
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
        Load { name: NameString, ddb_handle: SuperName<'a> },

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


    //
    // 5.4 Type 2 Opcodes
    //
    // From ASL grammar (19.2.5):
    //
    // > A Type 2 opcode returns a value and can be used in an expression.
    //

    // Type6Opcode         := DefRefOf | DefDerefOf | DefIndex | ??UserTermObj
    //
    // From ASL grammar (19.2.5):
    //
    // > The Type 6 opcodes are a subset of Type 2 opcodes that return a Reference value
    // > and can be used in an expression. They cannot be evaluated at compile time. Type
    // > 6 also includes the UserTerm, which is a control method invocation.
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

    // Type2Opcode := DefAcquire | DefAdd | DefAnd | DefBuffer | DefConcat |
    //                DefConcatRes | DefCondRefOf | DefCopyObject | DefDecrement |
    //                DefDerefOf | DefDivide | DefFindSetLeftBit | DefFindSetRightBit |
    //                DefFromBCD | DefIncrement | DefIndex | DefLAnd | DefLEqual |
    //                DefLGreater | DefLGreaterEqual | DefLLess | DefLLessEqual | DefMid |
    //                DefLNot | DefLNotEqual | DefLoadTable | DefLOr | DefMatch | DefMod |
    //                DefMultiply | DefNAnd | DefNOr | DefNot | DefObjectType | DefOr |
    //                DefPackage | DefVarPackage | DefRefOf | DefShiftLeft | DefShiftRight |
    //                DefSizeOf | DefStore | DefSubtract | DefTimer | DefToBCD | DefToBuffer |
    //                DefToDecimalString | DefToHexString | DefToInteger | DefToString |
    //                DefWait | DefXOr | MethodInvocation
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum ExpressionOpcode<'a> {
        RefExpression(ReferenceExpressionOpcode<'a>),
        Buffer(Buffer),
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

    // MatchOpcode         := ByteData
    //     // 0 MTR
    //     // 1 MEQ
    //     // 2 MLE
    //     // 3 MLT
    //     // 4 MGE
    //     // 5 MGT
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum MatchOpcode {
        True,
        Equal,
        LessEqual,
        Less,
        GreaterEqual,
        Greater,
    }

    // Table 19-433 in the ACPI spec:
    //   0   Uninitialized
    //   1   Integer
    //   2   String
    //   3   Buffer
    //   4   Package
    //   5   Field Unit
    //   6   Device
    //   7   Event
    //   8   Method
    //   9   Mutex
    //   10  Operation Region
    //   11  Power Resource
    //   12  Processor
    //   13  Thermal Zone
    //   14  Buffer Field
    //   15  DDB Handle
    //   16  Debug Object
    //   >16 Reserved
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



//
// 6. Miscellaneous Objects
//
pub mod misc {
    // ArgObj   := Arg0Op | Arg1Op | Arg2Op | Arg3Op | Arg4Op | Arg5Op | Arg6Op
    // Arg0Op   := 0x68
    // Arg1Op   := 0x69
    // Arg2Op   := 0x6A
    // Arg3Op   := 0x6B
    // Arg4Op   := 0x6C
    // Arg5Op   := 0x6D
    // Arg6Op   := 0x6E
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

    // LocalObj := Local0Op | Local1Op | Local2Op | Local3Op | Local4Op | Local5Op | Local6Op | Local7Op
    // Local0Op := 0x60
    // Local1Op := 0x61
    // Local2Op := 0x62
    // Local3Op := 0x63
    // Local4Op := 0x64
    // Local5Op := 0x65
    // Local6Op := 0x66
    // Local7Op := 0x67
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

    // DebugObj := DebugOp
    // DebugOp  := ExtOpPrefix 0x31
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct DebugObject { }
}
