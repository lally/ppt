#include "llvm/LLVMContext.h"
#include "llvm/Module.h"
#include "llvm/PassManager.h"
#include "llvm/LinkAllPasses.h"
#include "llvm/LinkAllVMCore.h"
#include "llvm/Constants.h"
#include "llvm/Analysis/Verifier.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/ExecutionEngine/GenericValue.h"
#include "llvm/ExecutionEngine/JIT.h"
#include "llvm/Target/TargetSelect.h"cat
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/STLExtras.h"
#include <iostream>
#include <fstream>
#include <string.h>

using namespace llvm;


int main(int args, char ** argv) {
  cl::ParseCommandLineOptions(args, argv, " perftrace generator\n");

  LLVMContext& ctx = getGlobalContext();

  raw_ostream *out = &outs();

  Module * mod = new Module("perftrace_reader", ctx);
  Constant * puts = mod->getOrInsertFunction("puts",
					     IntegerType::getInt32Ty(ctx),
					     PointerType::getUnqual(
					       IntegerType::getInt8Ty(ctx)), NULL);
					       
  Function * _main = 
    cast<Function>(mod->getOrInsertFunction("main", 
					    IntegerType::getInt32Ty(ctx),
					    IntegerType::getInt32Ty(ctx),
					    PointerType::getUnqual(
					      PointerType::getUnqual(
						IntegerType::getInt8Ty(ctx))), NULL));
  Function::arg_iterator arrrgs = _main->arg_begin();
  arrrgs++->setName("argc");
  arrrgs++->setName("argv");

  BasicBlock *bb = BasicBlock::Create(ctx, "main.0", _main);
  char *message = "hello world!";
  const int len = strlen(message)+1;
  std::vector<Constant*> cst;
  for (int i=0; i<len; ++i) {
    cst.push_back(ConstantInt::get(IntegerType::getInt8Ty(ctx),
				   message[i],
				   true));
  }

  Constant *str = ConstantArray::get(ArrayType::get(IntegerType::getInt8Ty(ctx), cst.size()),
				     &cst[0], cst.size());
  Constant * index = ConstantInt::get(IntegerType::getInt32Ty(ctx), 0, false);
  Constant *arg = ConstantExpr::getGetElementPtr(str, 
						 &index,
						 1);			     
  Value * argvec[] = { arg };
  CallInst *puts_call = CallInst::Create(puts, argvec, argvec + 1, "", bb);
  puts_call->setTailCall(false);
  ReturnInst::Create(ctx, ConstantInt::get(ctx, APInt(32, 0)), bb);

  WriteBitcodeToFile(mod, *out);

  delete mod;
  llvm_shutdown();

  return 0;
}
