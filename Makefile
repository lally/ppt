
CXXFLAGS=-I/usr/local/include -D__STDC_LIMIT_MACROS -D__STDC_CONSTANT_MACROS

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -g -c $< -o $@
all: test1 test2 libmet_listener test_llvm


libmet_listener: libmet_listener.o
	g++ -o $@ $^

test2: test2.o
	g++ -o $@ $^

test1: test1.o
	g++ -o $@ $^


test_llvm: test_llvm.o
	g++ -g -o $@ $^ -L/usr/local/lib -lLLVMBitWriter -lLLVMARMAsmParser -lLLVMARMAsmPrinter -lLLVMARMCodeGen -lLLVMARMInfo -lLLVMAlphaAsmPrinter -lLLVMAlphaCodeGen -lLLVMAlphaInfo -lLLVMAnalysis -lLLVMArchive -lLLVMAsmParser -lLLVMAsmPrinter -lLLVMBitReader -lLLVMBitWriter -lLLVMBlackfinAsmPrinter -lLLVMBlackfinCodeGen -lLLVMBlackfinInfo -lLLVMCBackend -lLLVMCBackendInfo -lLLVMCellSPUAsmPrinter -lLLVMCellSPUCodeGen -lLLVMCellSPUInfo -lLLVMCodeGen -lLLVMCore -lLLVMCppBackend -lLLVMCppBackendInfo -lLLVMExecutionEngine -lLLVMInstCombine -lLLVMInstrumentation -lLLVMInterpreter -lLLVMJIT -lLLVMLinker -lLLVMMBlazeAsmPrinter -lLLVMMBlazeCodeGen -lLLVMMBlazeInfo -lLLVMMC -lLLVMMCParser -lLLVMMSIL -lLLVMMSILInfo -lLLVMMSP430AsmPrinter -lLLVMMSP430CodeGen -lLLVMMSP430Info -lLLVMMipsAsmPrinter -lLLVMMipsCodeGen -lLLVMMipsInfo -lLLVMPIC16AsmPrinter -lLLVMPIC16CodeGen -lLLVMPIC16Info -lLLVMPowerPCAsmPrinter -lLLVMPowerPCCodeGen -lLLVMPowerPCInfo -lLLVMScalarOpts -lLLVMSelectionDAG -lLLVMSparcAsmPrinter -lLLVMSparcCodeGen -lLLVMSparcInfo -lLLVMSupport -lLLVMSystem -lLLVMSystemZAsmPrinter -lLLVMSystemZCodeGen -lLLVMSystemZInfo -lLLVMTarget -lLLVMTransformUtils -lLLVMX86AsmParser -lLLVMX86AsmPrinter -lLLVMX86CodeGen -lLLVMX86Disassembler -lLLVMX86Info -lLLVMXCoreAsmPrinter -lLLVMXCoreCodeGen -lLLVMXCoreInfo -lLLVMipa -lLLVMipo -lLLVMpic16passes 
