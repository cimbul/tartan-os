//
// Syntax tests for scoping rules
//
DefinitionBlock("", "DSDT", 2, "Tartan", "Names", 0) {
	External(\OTHR)

	Device(\MTHS) {
		Method(MTH1, 1) { Return(Arg0) }

		MTH1(10)
		// ^MTH1(10)        // Error 6161 - does not exist
		// ^^MTH1(10)       // Error 6161 - does not exist
		// MTHS.MTH1(10)    // Error 6161 - does not exist
		^MTHS.MTH1(10)
		// ^^MTHS.MTH1(10)  // Error 6161 - does not exist
		// ^^^MTHS.MTH1(10) // Error 6161 - does not exist
		\MTHS.MTH1(10)

		Method(AAAA) {
			MTH1(10)
			^MTH1(10)
			// ^^MTH1(10)       // Error 6161 - does not exist
			// MTHS.MTH1(10)    // Error 6161 - does not exist
			// ^MTHS.MTH1(10)   // Error 6161 - does not exist
			^^MTHS.MTH1(10)
			// ^^^MTHS.MTH1(10) // Error 6161 - does not exist
			\MTHS.MTH1(10)
		}

		Method(^BBBB) {
			// MTH1(10)         // Error 6088 - not accessible from this scope
			// ^MTH1(10)        // Error 6161 - does not exist
			// ^^MTH1(10)       // Error 6161 - does not exist
			// MTHS.MTH1(10)    // Error 6161 - does not exist
			^MTHS.MTH1(10)
			// ^^MTHS.MTH1(10)  // Error 6161 - does not exist
			// ^^^MTHS.MTH1(10) // Error 6161 - does not exist
			\MTHS.MTH1(10)
		}

		External(NEST)
		Method(NEST.CCCC) {
			MTH1(10)
			// ^MTH1(10)        // Error 6161 - does not exist
			^^MTH1(10)
			// MTHS.MTH1(10)    // Error 6161 - does not exist
			// ^MTHS.MTH1(10)   // Error 6161 - does not exist
			// ^^MTHS.MTH1(10)  // Error 6161 - does not exist
			^^^MTHS.MTH1(10)
			\MTHS.MTH1(10)
		}

		Method(\DDDD) {
			// MTH1(10)         // Error 6088 - not accessible from this scope
			// ^MTH1(10)        // Error 6161 - does not exist
			// ^^MTH1(10)       // Error 6161 - does not exist
			// MTHS.MTH1(10)    // Error 6161 - does not exist
			^MTHS.MTH1(10)
			// ^^MTHS.MTH1(10)  // Error 6161 - does not exist
			// ^^^MTHS.MTH1(10) // Error 6161 - does not exist
			\MTHS.MTH1(10)
		}

		Method(\MTHS.EEEE) {
			MTH1(10)
			^MTH1(10)
			// ^^MTH1(10)       // Error 6161 - does not exist
			// MTHS.MTH1(10)    // Error 6161 - does not exist
			// ^MTHS.MTH1(10)   // Error 6161 - does not exist
			^^MTHS.MTH1(10)
			// ^^^MTHS.MTH1(10) // Error 6161 - does not exist
			\MTHS.MTH1(10)
		}

		Method(\OTHR.EEEE) {
			// MTH1(10)         // Error 6088 - not accessible from this scope
			// ^MTH1(10)        // Error 6161 - does not exist
			// ^^MTH1(10)       // Error 6161 - does not exist
			// MTHS.MTH1(10)    // Error 6161 - does not exist
			// ^MTHS.MTH1(10)   // Error 6161 - does not exist
			^^MTHS.MTH1(10)
			// ^^^MTHS.MTH1(10) // Error 6161 - does not exist
			\MTHS.MTH1(10)
		}
	}

	// MTH1(10)         // Error 6088 - not accessible from this scope
	// ^MTH1(10)        // Error 6161 - does not exist
	// ^^MTH1(10)       // Error 6161 - does not exist
	MTHS.MTH1(10)
	// ^MTHS.MTH1(10)   // Error 6161 - does not exist
	// ^^MTHS.MTH1(10)  // Error 6161 - does not exist
	// ^^^MTHS.MTH1(10) // Error 6161 - does not exist
	\MTHS.MTH1(10)
}
