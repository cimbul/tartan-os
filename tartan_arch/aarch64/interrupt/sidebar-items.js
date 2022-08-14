window.SIDEBAR_ITEMS = {"enum":[["Kind","The kind of exception being handled: sync, IRQ, FIQ, SError."],["Source","The exception level that generated an exception."],["VectorBaseAddressRegister","`VBAR_ELx`: Contains the address of the [`VectorTable`] that the processor should use at a given exception level."]],"struct":[["Class","Classifies different exception causes."],["MaskRegister","`DAIF`: Controls masking of different kinds of exceptions."],["SyndromeRegister","`ESR_ELx`: Holds information about the cause of the exception currently being handled."],["VectorEntry","A single entry in the exception [`VectorTable`]."],["VectorTable","Exception vector table that contains code to handle exceptions from each combination of [`Kind`] and [`Source`]."]]};