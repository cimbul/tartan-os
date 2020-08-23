//
// Named objects and data that do not involve "runtime" operations
//
DefinitionBlock("", "SSDT", 2, "Tartan", "Data", 0) {

	//
	// Data

	// TODO: Name with "DataRefObject" misc. alternatives
	Name(U8, 0xff)
	Name(U16, 0xffff)
	Name(U32, 0xffffffff)
	Name(U64, 0xfffffffffffffff)
	Name(STR0, "")
	Name(STR, "Hello, world!")
	Name(Z, Zero)
	Name(O, One)
	Name(F, Ones)

	Name(BUF0, Buffer() {})
	Name(BUFF, Buffer(0x10) { 0x01, 0x02, 0x03 })
	Name(VBUF, Buffer(\U32) {})

	Name(PKG0, Package() {})
	Name(PKG1, Package(0x20) {
		// Constants
		0xff,
		"foo",
		Ones,
		Buffer() { 0xff, 0xee },
		Package() {
			0xcafebabe,
			0xdeadbeef,
		},

		// Data references
		\U32,  // Integer
		\STR,  // String
		\BUFF, // Buffer
		\FLD3, // Field Unit
		\VPKG, // Package

		// Object References
		\DEV1, // Device
		\EVT1, // Event
		\MTH1, // Method
		\MTX1, // Mutex
		\REG1, // Operation Region
		\PWR1, // Power Resource
		\PRC1, // Processor
		\THZ1, // Thermal Zone
	})

	// Encoded as variable-length because size isn't known until runtime
	Name(VPKG, Package(\U32) {
		"stuff",
		Zero,
	})

	// Encoded as variable-length because size > 0xff
	Name(VPKG.OTHR, Package(0x100) {
		"other",
		0xabcd,
	})


	//
	// Named objects

	DataTableRegion(REG1, "XYZ1", "Tartan", "SomeTable")
	OperationRegion(REG2, GenericSerialBus, 0x42, 0x30)

	Name(COM1, ResourceTemplate() {
		I2CSerialBusV2(0x12,,96000,,"\\I2C",,,,,RawDataBuffer(){3,4})
	})

	BankField(REG2, FLD6, 0x1234, BufferAcc, Lock, Preserve) {
		FLD1, 10,
		FLD2, 4,
		Offset(24),
		Connection(COM1),
		AccessAs(BufferAcc, AttribBytes(4)),
		FLD3, 5,
		Connection(I2CSerialBusV2(0x12,,96000,,"\\I2C",,,,,RawDataBuffer(){3,4})),
	}
	Field(REG1, QWordAcc, Lock, WriteAsZeros) {}
	IndexField(FLD1, FLD2, DWordAcc, NoLock, WriteAsOnes) {}

	CreateField(BUFF, 3, 5, FLD4)
	CreateBitField(BUFF, 9, FLD5)
	CreateByteField(BUFF, 2, FLD6)
	CreateWordField(BUFF, 8, FLD7)
	CreateDWordField(BUFF, 4, FLD8)
	CreateQWordField(BUFF, 7, FLD9)

	External(\SOME.OBJ, MethodObj)
	Device(DEV1) {}
	Event(EVT1)
	Method(MTH1) {}
	Mutex(MTX1, 0)
	PowerResource(PWR1, 2, 99) {}
	Processor(PRC1, 1, 0x123, 10) {}
	ThermalZone(THZ1) {}


	//
	// Namespace modifiers

	Alias(MTH1, MTH2)

	Scope(DEV1) {
		Name(FOO, "bar")
	}
}
