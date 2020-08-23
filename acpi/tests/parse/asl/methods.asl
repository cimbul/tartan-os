//
// Syntax tests for edge cases of method calls/references
//
DefinitionBlock("", "DSDT", 2, "Tartan", "Methods", 0) {

	Name(\FOO, 123)
	Name(\BAR, 456)
	Name(\BAZ, 789)

	Device(\MTHS) {
		External(EXTN, MethodObj)
		Name(REF1, 0)
		Name(REF2, 0)
		Name(REF3, 0)
	}

	Method(\MTHS.MTH0)    { Return("Hello, world!") }
	Method(\MTHS.MTH1, 1) { Return(Arg0) }
	Method(\MTHS.MTH2, 2) { Return(Arg1) }


	// Simple method calls
	\MTHS.MTH0()
	\MTHS.MTH1(1)
	\MTHS.MTH2(1, 2)

	// Compiler infers arguments for external function based on first use
	\MTHS.EXTN(\FOO, \BAR)
	// \MTHS.EXTN()                 // Error 6005 - too few arguments
	// \MTHS.EXTN(\FOO)             // Error 6005 - too few arguments
	// \MTHS.EXTN(\FOO, \BAR, \BAZ) // Error 6004 - too many arguments

	// Cannot call externals with unknown type
	// \FOO(\BAR)  // Error 6086 - not a control method

	// Compiler tries to parse standalone names in TermObj as method calls, which leads to
	// errors on the next statement
	// \MTHS.MTH0  // Error 6126 - unexpected PARSEOP_ALIAS
	// \FOO        // Error 6126 - unexpected PARSEOP_ALIAS
	// Local0      // Error 6126 - unexpected PARSEOP_ALIAS
	// Debug       // Error 6126 - unexpected PARSEOP_ALIAS

	// Compiler does NOT treat method references/aliases as a callable
	Alias(\MTHS.MTH1, \MTHS.ALIS)
	Name(\MTHS.PKG, Package() { \MTHS.MTH1 })
	// \MTHS.REF1 = \MTHS.MTH1  // Parses MTH1 as a method call, which isn't what we want
	\MTHS.REF1 = RefOf(\MTHS.MTH1)
	\MTHS.REF2 = DerefOf(RefOf(\MTHS.MTH1))
	\MTHS.REF3 = DerefOf(\MTHS.PKG[0])
	// \MTHS.ALIS(99)  // Error 6086 - not a control method
	// \MTHS.REF1(99)  // Error 6086 - not a control method
	// \MTHS.REF2(99)  // Error 6086 - not a control method
	// \MTHS.REF3(99)  // Error 6086 - not a control method

	// !!! ...but the disassembler does ...sometimes !!!
	// TODO: What does interpreter do with this?
	// Method(TST) {
	// 	CopyObject(SizeOf(\MTHS.REF1), Local0)
	// 	CopyObject(SizeOf(\MTHS.ALIS), Local0)
	// 	CopyObject(SizeOf(\MTHS.MTH0), Local1)
	// }
}
