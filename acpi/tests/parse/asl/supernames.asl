//
// Syntax tests for SuperName and SimpleName productions
//
DefinitionBlock("", "DSDT", 2, "Tartan", "Names", 0) {
	External(\FOO)
	External(\BAR)
	External(\BAZ)
	External(\TST)

	External(\MTHS)
	External(\MTHS.EXTN, MethodObj, UnknownObj, {UnknownObj})
	Method(\MTHS.MTH0)    { Return(42)   }
	Method(\MTHS.MTH1, 1) { Return(Arg0) }



	///////////////////////
	// Normal SuperNames //
	///////////////////////



	//////////
	// Store (target)
	Method(\TST.STOR, 1) {
		Local0 = "Foo"

		// SimpleNames:
		Store(\FOO, \BAR)
		Store(\FOO, Arg0)
		Store(\FOO, Local0)

		// Ambiguous method references/calls:
		Store(\FOO, \MTHS.MTH0)          // !!! Treated as a call
		Store(\FOO, \MTHS.MTH0())

		// Method calls
		Store(\FOO, \MTHS.MTH1(\BAR))
		Store(\FOO, \MTHS.EXTN(\BAR))

		// Method references
		// Store(\FOO, \MTHS.MTH1)       // Error 6005 - too few arguments
		// Store(\FOO, \MTHS.EXTN)       // Error 6005 - too few arguments

		// Terms included in SuperName but not SimpleName:
		Store(\FOO, Debug)
		Store(\FOO, RefOf(\BAR))
		Store(\FOO, DerefOf(\BAR))
		Store(\FOO, Index(0, 1))

		// Terms not included in SuperName:
		// Store(\FOO, 0x10)             // Error 6126 - unexpected PARSEOP_INTEGER
		// Store(\FOO, Buffer() {0x10})  // Error 6126 - unexpected PARSEOP_BUFFER
		// Store(\FOO, LOr(\BAR, \BAZ))  // Error 6126 - unexpected PARSEOP_LOR
		// Store(\FOO, NoOp)             // Error 6126 - unexpected PARSEOP_NOOP
		// Store(\FOO, Timer)            // Error 6126 - unexpected PARSEOP_TIMER
		// Store(\FOO, CondRefOf(\BAR))  // Error 6126 - unexpected PARSEOP_CONDREFOF
	}


	//////////
	// Increment
	Method(\TST.INC, 1) {
		Local0 = "Foo"

		// SimpleNames:
		Increment(\BAR)
		Increment(Arg0)
		Increment(Local0)

		// Ambiguous method references/calls:
		Increment(\MTHS.MTH0)
		Increment(\MTHS.MTH0())

		// Method calls
		Increment(\MTHS.MTH1(\BAR))
		Increment(\MTHS.EXTN(\BAR))

		// Method references
		// Increment(\MTHS.MTH1)       // Error 6005 - too few arguments
		// Increment(\MTHS.EXTN)       // Error 6005 - too few arguments

		// Terms included in SuperName but not SimpleName:
		Increment(Debug)
		Increment(RefOf(\BAR))
		Increment(DerefOf(\BAR))
		Increment(Index(\BAR, 0))

		// Terms not included in SuperName:
		// Increment(0x10)             // Error 6126 - unexpected PARSEOP_INTEGER
		// Increment(Buffer() {0x10})  // Error 6126 - unexpected PARSEOP_BUFFER
		// Increment(LOr(\BAR, \BAZ))  // Error 6126 - unexpected PARSEOP_LOR
		// Increment(NoOp)             // Error 6126 - unexpected PARSEOP_NOOP
		// Increment(Timer)            // Error 6126 - unexpected PARSEOP_TIMER
		// Increment(CondRefOf(\BAR))  // Error 6126 - unexpected PARSEOP_CONDREFOF
	}


	//////////
	// Add (target)
	Method(\TST.ADD, 1) {
		Local0 = "Foo"

		// SimpleNames:
		Add(\FOO, 1, \BAR)
		Add(\FOO, 1, Arg0)
		Add(\FOO, 1, Local0)

		// Ambiguous method references/calls:
		Add(\FOO, 1, \MTHS.MTH0)
		Add(\FOO, 1, \MTHS.MTH0())

		// Method calls
		Add(\FOO, 1, \MTHS.MTH1(\BAR))
		Add(\FOO, 1, \MTHS.EXTN(\BAR))

		// Method references
		// Add(\FOO, 1, \MTHS.MTH1)       // Error 6005 - too few arguments
		// Add(\FOO, 1, \MTHS.EXTN)       // Error 6005 - too few arguments

		// Terms included in SuperName but not SimpleName:
		Add(\FOO, 1, Debug)
		Add(\FOO, 1, RefOf(\BAR))
		Add(\FOO, 1, DerefOf(\BAR))
		Add(\FOO, 1, Index(\BAR, 0))

		// Terms not included in SuperName:
		// Add(\FOO, 1, 0x10)             // Error 6126 - unexpected PARSEOP_INTEGER
		// Add(\FOO, 1, Buffer() {0x10})  // Error 6126 - unexpected PARSEOP_BUFFER
		// Add(\FOO, 1, LOr(\BAR, \BAZ))  // Error 6126 - unexpected PARSEOP_LOR
		// Add(\FOO, 1, NoOp)             // Error 6126 - unexpected PARSEOP_NOOP
		// Add(\FOO, 1, Timer)            // Error 6126 - unexpected PARSEOP_TIMER
		// Add(\FOO, 1, CondRefOf(\BAR))  // Error 6126 - unexpected PARSEOP_CONDREFOF
	}


	//////////
	// SizeOf
	Method(\TST.SZOF, 1) {
		Local0 = "Foo"

		// SimpleNames:
		SizeOf(\BAR)
		SizeOf(Arg0)
		SizeOf(Local0)

		// Ambiguous method references/calls:
		SizeOf(\MTHS.MTH0)
		SizeOf(\MTHS.MTH0())

		// Method calls
		SizeOf(\MTHS.MTH1(\BAR))
		SizeOf(\MTHS.EXTN(\BAR))

		// Method references
		// SizeOf(\MTHS.MTH1)       // Error 6005 - too few arguments
		// SizeOf(\MTHS.EXTN)       // Error 6005 - too few arguments

		// Terms included in SuperName but not SimpleName:
		SizeOf(Debug)
		SizeOf(RefOf(\BAR))
		SizeOf(DerefOf(\BAR))
		SizeOf(Index(\BAR, 0))

		// Terms not included in SuperName:
		// SizeOf(0x10)             // Error 6126 - unexpected PARSEOP_INTEGER
		// SizeOf(Buffer() {0x10})  // Error 6126 - unexpected PARSEOP_BUFFER
		// SizeOf(LOr(\BAR, \BAZ))  // Error 6126 - unexpected PARSEOP_LOR
		// SizeOf(NoOp)             // Error 6126 - unexpected PARSEOP_NOOP
		// SizeOf(Timer)            // Error 6126 - unexpected PARSEOP_TIMER
		// SizeOf(CondRefOf(\BAR))  // Error 6126 - unexpected PARSEOP_CONDREFOF
	}


	//////////
	// Index (target)
	Method(\TST.INDX, 1) {
		Local0 = "Foo"

		// SimpleNames:
		Index(\FOO, 1, \BAR)
		Index(\FOO, 1, Arg0)
		Index(\FOO, 1, Local0)

		// Ambiguous method references/calls:
		Index(\FOO, 1, \MTHS.MTH0)
		Index(\FOO, 1, \MTHS.MTH0())

		// Method calls
		Index(\FOO, 1, \MTHS.MTH1(\BAR))
		Index(\FOO, 1, \MTHS.EXTN(\BAR))

		// Method references
		// Index(\FOO, 1, \MTHS.MTH1)       // Error 6005 - too few arguments
		// Index(\FOO, 1, \MTHS.EXTN)       // Error 6005 - too few arguments

		// Terms included in SuperName but not SimpleName:
		Index(\FOO, 1, Debug)
		Index(\FOO, 1, RefOf(\BAR))
		Index(\FOO, 1, DerefOf(\BAR))
		Index(\FOO, 1, Index(\BAR, 0))

		// Terms not included in SuperName:
		// Index(\FOO, 1, 0x10)             // Error 6126 - unexpected PARSEOP_INTEGER
		// Index(\FOO, 1, Buffer() {0x10})  // Error 6126 - unexpected PARSEOP_BUFFER
		// Index(\FOO, 1, LOr(\BAR, \BAZ))  // Error 6126 - unexpected PARSEOP_LOR
		// Index(\FOO, 1, NoOp)             // Error 6126 - unexpected PARSEOP_NOOP
		// Index(\FOO, 1, Timer)            // Error 6126 - unexpected PARSEOP_TIMER
		// Index(\FOO, 1, CondRefOf(\BAR))  // Error 6126 - unexpected PARSEOP_CONDREFOF
	}

	//////////
	// Load (target)
	Method(\TST.LOAD, 1) {
		Local0 = "Foo"

		// SimpleNames:
		Load(\FOO, \BAR)
		Load(\FOO, Arg0)
		Load(\FOO, Local0)

		// Ambiguous method references/calls:
		Load(\FOO, \MTHS.MTH0)
		Load(\FOO, \MTHS.MTH0())

		// Method calls
		Load(\FOO, \MTHS.MTH1(\BAR))
		Load(\FOO, \MTHS.EXTN(\BAR))

		// Method references
		// Load(\FOO, \MTHS.MTH1)       // Error 6005 - too few arguments
		// Load(\FOO, \MTHS.EXTN)       // Error 6005 - too few arguments

		// Terms included in SuperName but not SimpleName:
		Load(\FOO, Debug)
		Load(\FOO, RefOf(\BAR))
		Load(\FOO, DerefOf(\BAR))
		Load(\FOO, Index(\BAR, 0))

		// Terms not included in SuperName:
		// Load(\FOO, 0x10)             // Error 6126 - unexpected PARSEOP_INTEGER
		// Load(\FOO, Buffer() {0x10})  // Error 6126 - unexpected PARSEOP_BUFFER
		// Load(\FOO, LOr(\BAR, \BAZ))  // Error 6126 - unexpected PARSEOP_LOR
		// Load(\FOO, NoOp)             // Error 6126 - unexpected PARSEOP_NOOP
		// Load(\FOO, Timer)            // Error 6126 - unexpected PARSEOP_TIMER
		// Load(\FOO, CondRefOf(\BAR))  // Error 6126 - unexpected PARSEOP_CONDREFOF
	}



	///////////////////////////////
	// Exceptions and edge cases //
	///////////////////////////////


	//////////
	// CopyObject takes SimpleName as its target, *not* SuperName
	Method(\TST.COPY, 1) {
		Local0 = "Foo"

		// SimpleNames:
		CopyObject(\FOO, \BAR)
		CopyObject(\FOO, Arg0)
		CopyObject(\FOO, Local0)

		// Ambiguous method references/calls:
		CopyObject(\FOO, \MTHS.MTH0)
		// CopyObject(\FOO, \MTHS.MTH0())     // Error 6126 - unexpected PARSEOP_OPEN_PAREN

		// Method calls:
		// CopyObject(\FOO, \MTHS.MTH1(\BAR)) // Error 6126 - unexpected PARSEOP_OPEN_PAREN
		// CopyObject(\FOO, \MTHS.EXTN(\BAR)) // Error 6126 - unexpected PARSEOP_OPEN_PAREN

		// Method references:
		// CopyObject(\FOO, \MTHS.MTH1)       // Error 6005 - too few arguments
		// CopyObject(\FOO, \MTHS.EXTN)       // Error 6005 - too few arguments

		// Terms included in SuperName but not SimpleName:
		// CopyObject(\FOO, Debug)            // Error 6126 - unexpected PARSEOP_DEBUG
		// CopyObject(\FOO, RefOf(\BAR))      // Error 6126 - unexpected PARSEOP_REFOF
		// CopyObject(\FOO, DerefOf(\BAR))    // Error 6126 - unexpected PARSEOP_DEREFOF
		// CopyObject(\FOO, Index(\BAR, 0))   // Error 6126 - unexpected PARSEOP_INDEX

		// Terms not included in SuperName:
		// CopyObject(\FOO, 0x10)             // Error 6126 - unexpected PARSEOP_INTEGER
		// CopyObject(\FOO, Buffer() {0x10})  // Error 6126 - unexpected PARSEOP_BUFFER
		// CopyObject(\FOO, LOr(\BAR, \BAZ))  // Error 6126 - unexpected PARSEOP_LOR
		// CopyObject(\FOO, NoOp)             // Error 6126 - unexpected PARSEOP_NOOP
		// CopyObject(\FOO, Timer)            // Error 6126 - unexpected PARSEOP_TIMER
		// CopyObject(\FOO, CondRefOf(\BAR))  // Error 6126 - unexpected PARSEOP_CONDREFOF
	}


	//////////
	// RefOf takes SuperName *except* RefOf or method calls
	Method(\TST.REF, 1) {
		Local0 = "Foo"

		// SimpleNames:
		RefOf(\BAR)
		RefOf(Arg0)
		RefOf(Local0)

		// Ambiguous method references/calls:
		RefOf(\MTHS.MTH0)
		// RefOf(\MTHS.MTH0())     // Error 6126 - unexpected PARSEOP_CLOSE_PAREN

		// Method calls
		// RefOf(\MTHS.MTH1(\BAR)) // Error 6126 - unexpected PARSEOP_CLOSE_PAREN
		// RefOf(\MTHS.EXTN(\BAR)) // Error 6126 - unexpected PARSEOP_CLOSE_PAREN

		// Method references
		RefOf(\MTHS.MTH1)
		RefOf(\MTHS.EXTN)

		// Terms included in SuperName but not SimpleName:
		RefOf(Debug)
		// RefOf(RefOf(\BAR))      // Error 6126 - unexpected PARSEOP_CLOSE_PAREN
		RefOf(DerefOf(\BAR))
		RefOf(Index(\BAR, 0))

		// Terms not included in SuperName:
		// RefOf(0x10)             // Error 6126 - unexpected PARSEOP_INTEGER
		// RefOf(Buffer() {0x10})  // Error 6126 - unexpected PARSEOP_BUFFER
		// RefOf(LOr(\BAR, \BAZ))  // Error 6126 - unexpected PARSEOP_LOR
		// RefOf(NoOp)             // Error 6126 - unexpected PARSEOP_NOOP
		// RefOf(Timer)            // Error 6126 - unexpected PARSEOP_TIMER
		// RefOf(CondRefOf(\BAR))  // Error 6126 - unexpected PARSEOP_CONDREFOF
	}


	//////////
	// CondRefOf takes SuperName *except* RefOf or method calls
	Method(\TST.CREF, 1) {
		Local0 = "Foo"

		// SimpleNames:
		CondRefOf(\BAR)
		CondRefOf(Arg0)
		CondRefOf(Local0)

		// Ambiguous method references/calls:
		CondRefOf(\MTHS.MTH0)
		// CondRefOf(\MTHS.MTH0())     // Error 6126 - unexpected PARSEOP_CLOSE_PAREN

		// Method calls
		// CondRefOf(\MTHS.MTH1(\BAR)) // Error 6126 - unexpected PARSEOP_CLOSE_PAREN
		// CondRefOf(\MTHS.EXTN(\BAR)) // Error 6126 - unexpected PARSEOP_CLOSE_PAREN

		// Method references
		CondRefOf(\MTHS.MTH1)
		CondRefOf(\MTHS.EXTN)

		// Terms included in SuperName but not SimpleName:
		CondRefOf(Debug)
		// CondRefOf(RefOf(\BAR))      // Error 6126 - unexpected PARSEOP_CLOSE_PAREN
		CondRefOf(DerefOf(\BAR))
		CondRefOf(Index(\BAR, 0))

		// Terms not included in SuperName:
		// CondRefOf(0x10)             // Error 6126 - unexpected PARSEOP_INTEGER
		// CondRefOf(Buffer() {0x10})  // Error 6126 - unexpected PARSEOP_BUFFER
		// CondRefOf(LOr(\BAR, \BAZ))  // Error 6126 - unexpected PARSEOP_LOR
		// CondRefOf(NoOp)             // Error 6126 - unexpected PARSEOP_NOOP
		// CondRefOf(Timer)            // Error 6126 - unexpected PARSEOP_TIMER
		// CondRefOf(CondRefOf(\BAR))  // Error 6126 - unexpected PARSEOP_CONDREFOF
	}


	//////////
	// ObjectType takes SuperName *except* method calls as its target
	Method(\TST.TYPE, 1) {
		Local0 = "Foo"

		// SimpleNames:
		ObjectType(\BAR)
		ObjectType(Arg0)
		ObjectType(Local0)

		// Ambiguous method references/calls:
		ObjectType(\MTHS.MTH0)
		// ObjectType(\MTHS.MTH0())     // Error 6126 - unexpected PARSEOP_OPEN_PAREN

		// Method calls:
		// ObjectType(\MTHS.MTH1(\BAR)) // Error 6126 - unexpected PARSEOP_OPEN_PAREN
		// ObjectType(\MTHS.EXTN(\BAR)) // Error 6126 - unexpected PARSEOP_OPEN_PAREN

		// Method references:
		ObjectType(\MTHS.MTH1)
		ObjectType(\MTHS.EXTN)

		// Terms included in SuperName but not SimpleName:
		ObjectType(Debug)
		ObjectType(RefOf(\BAR))
		ObjectType(DerefOf(\BAR))
		ObjectType(Index(\BAR, 0))

		// Terms not included in SuperName:
		// ObjectType(0x10)             // Error 6126 - unexpected PARSEOP_INTEGER
		// ObjectType(Buffer() {0x10})  // Error 6126 - unexpected PARSEOP_BUFFER
		// ObjectType(LOr(\BAR, \BAZ))  // Error 6126 - unexpected PARSEOP_LOR
		// ObjectType(NoOp)             // Error 6126 - unexpected PARSEOP_NOOP
		// ObjectType(Timer)            // Error 6126 - unexpected PARSEOP_TIMER
		// ObjectType(CondRefOf(\BAR))  // Error 6126 - unexpected PARSEOP_CONDREFOF
	}
}
